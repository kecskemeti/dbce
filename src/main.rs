/*
 *  ========================================================================
 *  DBCE chess bot, lichess comms
 *  ========================================================================
 *
 *  This file is part of DBCE.
 *
 *  DBCE is free software: you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License as published
 *  by the Free Software Foundation, either version 3 of the License, or (at
 *  your option) any later version.
 *
 *  DBCE is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with DBCE.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  (C) Copyright 2022, Gabor Kecskemeti
 */
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::io;
use std::thread::sleep;
use std::time::{Duration, Instant};

use futures_util::StreamExt;
use rand::{thread_rng, Rng};
use reqwest::header::HeaderMap;
use reqwest::Client;

use crate::bitboard::{Engine, PSBoard, PieceColor};

#[global_allocator]
static GLOBAL: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

mod bitboard;
mod humaninteractions;

async fn play_a_game(
    gameid: &str,
    botid: &str,
    client: &Client,
) -> Result<Option<String>, Box<dyn Error>> {
    let resignwithgameid = format!("https://lichess.org/api/bot/game/{}/resign/", gameid);
    let movewithgameid = format!("https://lichess.org/api/bot/game/{}/move/", gameid);
    let getrq = client.get(format!(
        "https://lichess.org/api/bot/game/stream/{}",
        gameid
    ));
    let mut resp = getrq.send().await?.bytes_stream();
    let mut ourcolor = None;
    let mut prevopponentmove = Instant::now();
    let mut ourmovetime = 3000;
    let mut currentboard = PSBoard::new();
    let mut opponent = None;
    let mut toignore = None;
    let mut impossiblemove = None;
    let mut mindepth = 4;
    let mut depth = mindepth;
    let mut engine = Engine::new();
    while let Some(bytes) = resp.next().await {
        if let Ok(gamestate) = json::parse(String::from_utf8(Vec::from(bytes?.as_ref()))?.as_str())
        {
            let gamestate = if gamestate["type"] == "gameFull" {
                assert_eq!(gamestate["variant"]["short"], "Std");
                assert_eq!(gamestate["initialFen"], "startpos");
                if gamestate["speed"] == "ultraBullet" {
                    mindepth = 1;
                } else if gamestate["speed"] == "bullet" {
                    mindepth = 2;
                } else if gamestate["speed"] == "blitz" {
                    mindepth = 3;
                } else if gamestate["speed"] == "rapid" {
                    mindepth = 4;
                } else if gamestate["speed"] == "classical" {
                    mindepth = 5;
                } else if gamestate["speed"] == "correspondace" {
                    mindepth = 6;
                }
                let whiteplayer = gamestate["white"]["id"].as_str().unwrap();
                let blackplayer = gamestate["black"]["id"].as_str().unwrap();
                if whiteplayer.contains(botid) {
                    ourcolor = Some(PieceColor::White);
                    opponent = Some(String::from(blackplayer));
                } else if blackplayer.contains(botid) {
                    ourcolor = Some(PieceColor::Black);
                    opponent = Some(String::from(whiteplayer));
                } else {
                    println!("WARNING: We are not even playing the game {} !", gameid);
                    break;
                }
                &gamestate["state"]
            } else {
                &gamestate
            };
            if gamestate["type"] == "gameState" {
                if gamestate["status"] == "started" || gamestate["status"] == "created" {
                    if let Some(impossiblemove) = impossiblemove {
                        client.post(resignwithgameid).send().await?;
                        panic!(
                            "Could not make the following move {} on the board:\n {} \n {}",
                            impossiblemove,
                            currentboard,
                            gamestate["moves"].dump()
                        );
                    }
                    let white_rem_time = gamestate["wtime"].as_u64().unwrap() as u128;
                    let black_rem_time = gamestate["btime"].as_u64().unwrap() as u128;
                    currentboard = PSBoard::new();
                    let mut allmoves = gamestate["moves"].dump();
                    allmoves.retain(|c| c != '"');
                    for amove in allmoves.split_ascii_whitespace() {
                        currentboard = currentboard.make_an_uci_move(amove);
                    }
                    if currentboard.who_moves == *ourcolor.as_ref().unwrap() {
                        let our_rem_time = if currentboard.who_moves == PieceColor::White {
                            white_rem_time
                        } else {
                            black_rem_time
                        };
                        // it is our turn, let's see what we can come up with
                        unsafe {
                            bitboard::PSBCOUNT = 0;
                        }
                        let ins = Instant::now();
                        println!("Using {} depth for search", depth);
                        let mymove = engine.best_move_for(&currentboard, depth, true);
                        ourmovetime = ins.elapsed().as_millis();
                        unsafe {
                            println!("{} kNodes/sec", bitboard::PSBCOUNT as u128 / ourmovetime);
                        }
                        if (our_rem_time / ourmovetime) > 100 {
                            depth += 1;
                        } else if (our_rem_time / ourmovetime) < 25 {
                            depth = (depth - 1).max(mindepth);
                        }
                        println!("Move took {} ms", ourmovetime);
                        for _ in 0..5 {
                            if let Ok(res) = client
                                .post(format!("{}{}", movewithgameid, mymove.0.as_ref().unwrap()))
                                .send()
                                .await
                            {
                                if res.status().is_success() {
                                    impossiblemove = None;
                                    break;
                                }
                            }
                            println!("Move was not possible to send, retry..");
                            sleep(Duration::from_millis(500));
                            impossiblemove = Some(mymove.0.as_ref().unwrap().clone());
                        }
                    } else {
                        // This was sent to us to inform about the opponent, we just remember when this
                        // happened so we can decide what to do if the
                        prevopponentmove = Instant::now();
                    }
                } else {
                    // The game has most likely ended in a mate etc.
                    println!(
                        "Cannot play the game {}, as its status is: {}",
                        gameid, gamestate["status"]
                    );
                    break;
                }
            }
            // Chat is not supported
        } else {
            // Unparsable json
            if (prevopponentmove.elapsed().as_millis() as i128 - ourmovetime as i128) > 300000 {
                // we have not had a move from our opponent for over 5 mins.
                // we will just cancel the game
                let op = format!(
                    "https://lichess.org/api/bot/game/{}/{}",
                    gameid,
                    if currentboard.half_moves_since_pawn == 0 {
                        toignore = opponent;
                        println!("Due to initial inactivity aborting the game, we will move on to another opponent");
                        "abort"
                    } else {
                        println!("Due to midgame inactivity resigning the game, we will move on to another opponent");
                        "resign"
                    }
                );
                client.post(op).send().await?;
                break;
            }
        }
    }
    Ok(toignore)
}

/*
When running this bot, you first need to create a lichess bot account, then ask for an authorization
token for it. After these you can just run this executable and let it play. It will respond to
challenges (one at a time), also it will challenge bots currently available on lichess. By default
it does a 2.5 move deep full search.
 */
#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let input = io::stdin();
    println!("What is the auth token?");
    let mut line = String::new();
    {
        input.read_line(&mut line).unwrap();
    }
    let authtoken = line.trim();
    println!("What is the bot id?");
    let mut line = String::new();
    {
        input.read_line(&mut line).unwrap();
    }
    let botid = line.trim();
    println!("Try resuming previous game?");
    let mut line = String::new();
    {
        input.read_line(&mut line).unwrap();
    }
    let resume = line.trim();
    let mut headers = HeaderMap::new();
    headers.insert(
        "Authorization",
        format!("Bearer {}", authtoken).parse().unwrap(),
    );
    let client = Client::builder().default_headers(headers).build().unwrap();
    let mut declining_bots = HashSet::new();

    let mut gameid = None;
    if resume.to_lowercase().starts_with('y') {
        println!("Searching for previous, unfinished games..");
        let currentlyplaying = client
            .get("https://lichess.org/api/account/playing")
            .send()
            .await?;
        let agame = &json::parse(currentlyplaying.text().await?.as_str())?["nowPlaying"][0];
        if !agame.is_null() {
            println!("Resuming game...");
            gameid = Some(String::from(agame["gameId"].as_str().unwrap()));
        } else {
            println!("\tdid not find any.")
        }
    }
    loop {
        if let Some(gameid_str) = &gameid {
            println!("Starting to play game {}", gameid_str);
            let result = play_a_game(gameid_str.as_str(), botid, &client).await?;
            // If we get a non-responsive opponent we ignore it from now on
            if let Some(problematicopponent) = result {
                declining_bots.insert(problematicopponent);
            }
            gameid = None;
        } else {
            println!("No game at the moment");
            if thread_rng().gen_bool(0.5) {
                let mut resp = client
                    .get("https://lichess.org/api/bot/online")
                    .send()
                    .await?
                    .bytes_stream();
                println!("Searching for bots: ");
                let mut bots = Vec::with_capacity(1000);
                while let Some(bytes) = resp.next().await {
                    for abot_json in String::from_utf8(Vec::from(bytes?.as_ref()))?.lines() {
                        if let Ok(abot) = json::parse(abot_json) {
                            let botid = format!("{}", abot["id"]);
                            if !declining_bots.contains(botid.as_str()) {
                                bots.push(botid);
                            }
                        }
                    }
                }
                let target_bot = bots.swap_remove(thread_rng().gen_range(0..bots.len()) as usize);
                static CHALLENGE_TIME_CONTROLS: [(&str, &str); 6] = [
                    ("600", "10"),
                    ("300", "5"),
                    ("180", "3"),
                    ("60", "1"),
                    ("30", "1"),
                    ("15", "0"),
                ];
                let (current_limit, current_increment) = CHALLENGE_TIME_CONTROLS
                    [thread_rng().gen_range(0..CHALLENGE_TIME_CONTROLS.len())];
                let req_form = HashMap::from([
                    ("rated", "true"),
                    ("clock.limit", current_limit),
                    ("clock.increment", current_increment),
                    ("color", "random"),
                    ("variant", "standard"),
                ]);
                let post_req = client
                    .post(format!("https://lichess.org/api/challenge/{}", target_bot))
                    .form(&req_form);
                println!("Challenging bot: {}", target_bot);
                post_req.send().await?;
            }
            println!("Waiting for events:");
            {
                let mut events = client
                    .get("https://lichess.org/api/stream/event")
                    .send()
                    .await?
                    .bytes_stream();
                let startwait = Instant::now();
                'outer: while let Some(bytes) = events.next().await {
                    for eventupdate in String::from_utf8(Vec::from(bytes?.as_ref()))?.lines() {
                        if let Ok(event) = json::parse(eventupdate) {
                            if event["type"] == "gameStart" {
                                gameid = Some(String::from(event["game"]["id"].as_str().unwrap()));
                                break 'outer;
                            } else if event["type"] == "challenge" {
                                let event = &event["challenge"];
                                let mut reason = None;
                                if event["variant"]["key"] != "standard" {
                                    reason = Some("standard");
                                } else if event["rated"] != true {
                                    reason = Some("rated");
                                }
                                let mut postreq = client.post(format!(
                                    "https://lichess.org/api/challenge/{}/{}",
                                    event["id"].as_str().unwrap(),
                                    if reason.is_some() {
                                        "decline"
                                    } else {
                                        "accept"
                                    }
                                ));
                                if let Some(reason) = reason {
                                    println!("Declining challenge because: {}", reason);
                                    let reasonmap = HashMap::from([("reason", reason)]);
                                    postreq = postreq.form(&reasonmap);
                                } else {
                                    println!(
                                        "Challange accepted from {}",
                                        event["challenger"]["id"].as_str().unwrap()
                                    );
                                }
                                postreq.send().await?;
                            } else if event["type"] == "challengeDeclined" {
                                let decliner =
                                    event["challenge"]["destUser"]["id"].as_str().unwrap();
                                println!(
                                    "Challange declined by {} due to {}",
                                    decliner,
                                    event["challenge"]["declineReason"].as_str().unwrap()
                                );
                                declining_bots.insert(String::from(decliner));
                            }
                        }
                    }
                    if startwait.elapsed().as_secs() > 60 {
                        // If nothing interesting happened for 60 secs, we will try to do a challenge ourselves
                        break 'outer;
                    }
                }
            }
        }
    }
}
