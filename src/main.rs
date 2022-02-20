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

use crate::bitboard::{best_move_for, PSBoard, PieceColor};

#[global_allocator]
static GLOBAL: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

mod bitboard;

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
    let mut ourmovetime = 0;
    let mut currentboard = PSBoard::new();
    let mut opponent = None;
    let mut toignore = None;
    let mut impossoblemove = None;
    while let Some(bytes) = resp.next().await {
        if let Ok(gamestate) = json::parse(String::from_utf8(Vec::from(bytes?.as_ref()))?.as_str())
        {
            let gamestate = if gamestate["type"] == "gameFull" {
                assert!(gamestate["variant"]["short"] == "Std");
                assert!(gamestate["initialFen"] == "startpos");
                assert!(gamestate["speed"] != "ultraBullet");
                assert!(gamestate["speed"] != "bullet");
                assert!(gamestate["speed"] != "blitz");
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
                    if let Some(impossoblemove) = impossoblemove {
                        client.post(resignwithgameid).send().await?;
                        panic!(
                            "Could not make the following move {} on the board:\n {} \n {}",
                            impossoblemove,
                            currentboard,
                            gamestate["moves"].dump()
                        );
                    }

                    currentboard = PSBoard::new();
                    let mut allmoves = gamestate["moves"].dump();
                    allmoves.retain(|c| c != '"');
                    for amove in allmoves.split_ascii_whitespace() {
                        currentboard = currentboard.make_an_uci_move(amove);
                    }
                    if currentboard.who_moves == *ourcolor.as_ref().unwrap() {
                        // it is our turn, let's see what we can come up with
                        let ins = Instant::now();
                        let mymove = best_move_for(&currentboard, 4, true);
                        ourmovetime = ins.elapsed().as_millis();
                        println!("Move took {} ms", ourmovetime);
                        let mut res = None;
                        for _ in 0..5 {
                            res = Some(
                                client
                                    .post(format!(
                                        "{}{}",
                                        movewithgameid,
                                        mymove.0.as_ref().unwrap()
                                    ))
                                    .send()
                                    .await?,
                            );
                            if let Some(res) = res.as_ref() {
                                if res.status().is_success() {
                                    break;
                                }
                            }
                            println!("Move was not possible to send, retry..");
                            sleep(Duration::from_millis(500));
                        }
                        if let Some(res) = res {
                            if !res.status().is_success() {
                                impossoblemove = Some(mymove.0.unwrap());
                            }
                        } else {
                            // This should not really happen
                            panic!(
                                "Seriously could not make the following move {} on the board:\n {}",
                                mymove.0.unwrap(),
                                currentboard
                            );
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
            if prevopponentmove.elapsed().as_secs() > 300 {
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
    let mut headers = HeaderMap::new();
    headers.insert(
        "Authorization",
        format!("Bearer {}", authtoken).parse().unwrap(),
    );
    let client = reqwest::Client::builder()
        .default_headers(headers)
        .build()
        .unwrap();
    let mut declining_bots = HashSet::new();
    loop {
        println!("Searching for previous, unfinished games..");
        let currentlyplaying = client
            .get("https://lichess.org/api/account/playing")
            .send()
            .await?;
        let agame = &json::parse(currentlyplaying.text().await?.as_str())?["nowPlaying"][0];
        let mut gameid = None;
        if !agame.is_null() {
            println!("Resuming game...");
            gameid = Some(String::from(agame["gameId"].as_str().unwrap()));
        } else {
            println!("\tdid not find any.")
        }
        if gameid.is_none() {
            if thread_rng().gen_bool(0.2) {
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
                let targetbot = bots.swap_remove(thread_rng().gen_range(0..bots.len()) as usize);
                let reqform = HashMap::from([
                    ("rated", "true"),
                    ("clock.limit", "600"),
                    ("clock.increment", "10"),
                    ("color", "random"),
                    ("variant", "standard"),
                ]);
                let postreq = client
                    .post(format!("https://lichess.org/api/challenge/{}", targetbot))
                    .form(&reqform);
                println!("Challenging bot: {}", targetbot);
                postreq.send().await?;
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
                                    reason = Some("variant");
                                } else if event["rated"] != true {
                                    reason = Some("casual");
                                } else if event["speed"] == "ultraBullet"
                                    || event["speed"] == "bullet"
                                    || event["speed"] == "blitz"
                                {
                                    reason = Some("tooFast");
                                } else if event["speed"] == "correspondance" {
                                    reason = Some("tooSlow");
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
        if let Some(gameid) = gameid {
            println!("Starting to play game");
            let result = play_a_game(gameid.as_str(), botid, &client).await?;
            // If we get a non-responsive opponent we ignore it from now on
            if let Some(problematicopponent) = result {
                declining_bots.insert(problematicopponent);
            }
        } else {
            println!("No game at the moment");
        }
    }
}
