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
 *  (C) Copyright 2022-3, Gabor Kecskemeti
 */
use std::collections::{HashMap, HashSet};
use std::io;
use std::thread::sleep;
use std::time::{Duration, Instant};

use futures_util::StreamExt;
use rand::{thread_rng, Rng};
use reqwest::header::HeaderMap;
use reqwest::{Client, RequestBuilder, Response, StatusCode};

use dbce::baserules::piece_color::PieceColor::{Black, White};
use dbce::engine::Engine;
use dbce::human_facing::helper;
use dbce::util::{DurationAverage, EmptyResult, IntResult};
use serde_json::Value;

async fn play_a_game(gameid: &str, botid: &str, client: &Client) -> IntResult<Option<String>> {
    let resignwithgameid = format!("https://lichess.org/api/bot/game/{gameid}/resign/");
    let movewithgameid = format!("https://lichess.org/api/bot/game/{gameid}/move/");
    let getrq = client.get(format!("https://lichess.org/api/bot/game/stream/{gameid}"));
    let mut resp = lichess_api_call(getrq).await?.bytes_stream();
    let mut ourcolor = None;
    let mut prevopponentmove = Instant::now();
    let mut ourmovetime = Duration::from_secs(3);
    let mut opponent = None;
    let mut toignore = None;
    let mut impossiblemove = None;
    let (engine, mut state) = Engine::new();
    let mut lichesstiming = DurationAverage::new(50, || Duration::from_secs(1));
    while let Some(Ok(bytes)) = resp.next().await {
        let start = Instant::now();
        if let Ok(gamestate) = serde_json::from_slice(&bytes) {
            let gamestate: Value = gamestate;
            let gamestate = if gamestate["type"] == "gameFull" {
                assert_eq!(gamestate["variant"]["short"], "Std");
                assert_eq!(gamestate["initialFen"], "startpos");
                let whiteplayer = gamestate["white"]["id"].as_str().unwrap();
                let blackplayer = gamestate["black"]["id"].as_str().unwrap();
                if whiteplayer.contains(botid) {
                    ourcolor = Some(White);
                    opponent = Some(String::from(blackplayer));
                } else if blackplayer.contains(botid) {
                    ourcolor = Some(Black);
                    opponent = Some(String::from(whiteplayer));
                } else {
                    println!("WARNING: We are not even playing the game {gameid}!");
                    break;
                }
                &gamestate["state"]
            } else {
                &gamestate
            };
            if gamestate["type"] == "gameState" {
                if gamestate["status"] == "started" || gamestate["status"] == "created" {
                    if impossiblemove.is_some() {
                        lichess_api_call(client.post(resignwithgameid.clone())).await?;
                        panic!(
                            "Could not make the following move {impossiblemove:?} on the board:\n {} \n {}",
                            state.psboard(),
                            gamestate["moves"]
                        );
                    }
                    let white_rem_time = gamestate["wtime"].as_u64().unwrap();
                    let black_rem_time = gamestate["btime"].as_u64().unwrap();
                    let mut allmoves = gamestate["moves"].as_str().unwrap().to_owned();
                    allmoves.retain(|c| c != '"');
                    if let Some(lastmove) = allmoves.split_ascii_whitespace().last() {
                        state.make_an_uci_move(lastmove).await?;
                    }
                    let currentboard = state.psboard();
                    // we make sure we still have at least 20 moves to do before we run out of time.
                    let deadline_divisor = 20
                        * if currentboard.move_count == 0 {
                            10 // Make the first move very quick to avoid aborts
                        } else if currentboard.move_count < 10 {
                            2 // Make the next few moves a bit quicker to allow more thought in late games
                        } else {
                            1 // Let's just allow as much thought now as we can go for
                        };
                    let detected_color = *ourcolor.as_ref().unwrap();
                    if currentboard.who_moves == detected_color {
                        let our_rem_time = (i128::from(if currentboard.who_moves == White {
                            white_rem_time
                        } else {
                            black_rem_time
                        }) - lichesstiming.calc_average().as_millis() as i128)
                            .max(1) as u64;

                        let deadline =
                            Duration::from_millis(1.max(our_rem_time / deadline_divisor).min(5000));

                        for _ in 0..3 {
                            // it is our turn, let's see what we can come up with
                            let (dur, mymove) =
                                helper::calculate_move_for_console(&engine, &mut state, &deadline)
                                    .await;
                            ourmovetime = dur;
                            if detected_color.is_this_resignable(mymove.1) {
                                println!("Resign...");
                                lichess_api_call(client.post(resignwithgameid.clone())).await?;
                                break;
                            }
                            for _ in 0..5 {
                                if let Ok(res) = lichess_api_call(client.post(format!(
                                    "{}{}",
                                    movewithgameid,
                                    mymove.0.as_ref().unwrap()
                                )))
                                .await
                                {
                                    if res.status().is_success() {
                                        impossiblemove = None;
                                        break;
                                    } else if let Ok(res) = lichess_api_call(
                                        client.get("https://lichess.org/api/account/playing"),
                                    )
                                    .await
                                    {
                                        let gamelist = res.text().await.unwrap();
                                        let games: Value = serde_json::from_str(&gamelist).unwrap();
                                        if games["nowPlaying"].as_array().unwrap().is_empty() {
                                            impossiblemove = None;
                                            println!("Game is not even ongoing anymore, should not send moves!");
                                            break;
                                        }
                                    }
                                }
                                println!("Move was not possible to send, retry..");
                                sleep(Duration::from_millis(500));
                                impossiblemove = Some(*mymove.0.as_ref().unwrap());
                            }
                            if impossiblemove.is_some() {
                                println!("We will try another move, as this one did not work out");
                            } else {
                                break;
                            }
                        }
                        if impossiblemove.is_some() {
                            println!(
                                "Could not make the move, let's see if game status has changed"
                            );
                        }
                    } else {
                        // This was sent to us to inform about the opponent, we just remember when this
                        // happened so we can decide what to do if there is a parsing error
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
            // Unparsable json, this is most likely a lichess 6 second refresh
            static FIVE_MINS: Duration = Duration::from_secs(300);
            if (prevopponentmove.elapsed().saturating_sub(ourmovetime)) > FIVE_MINS {
                // we have not had a move from our opponent for over 5 mins.
                // we will just cancel the game
                let op = format!(
                    "https://lichess.org/api/bot/game/{}/{}",
                    gameid,
                    if state.psboard().half_moves_since_pawn == 0 {
                        toignore = opponent;
                        println!("Due to initial inactivity aborting the game, we will move on to another opponent");
                        "abort"
                    } else {
                        println!("Due to midgame inactivity resigning the game, we will move on to another opponent");
                        "resign"
                    }
                );
                lichess_api_call(client.post(op)).await?;
                break;
            }
        }
        lichesstiming.add(start.elapsed().saturating_sub(ourmovetime));
    }
    Ok(toignore)
}

async fn lichess_api_call(client_op: RequestBuilder) -> IntResult<Response> {
    let resp = client_op.send().await?;
    if resp.status() == StatusCode::TOO_MANY_REQUESTS {
        println!("Lichess rate limiting request, obeying with a bit more than a minute long delay");
        sleep(Duration::from_secs(61));
    }
    Ok(resp)
}

async fn best_rating_on_lichess(user: &str, client: &Client) -> IntResult<Option<u64>> {
    let target_props: Value =
        lichess_api_call(client.get(format!("https://lichess.org/api/user/{user}")))
            .await?
            .json()
            .await?;
    let user_performances = &target_props["perfs"];
    static PERF_TYPES: [&str; 4] = ["ultraBullet", "blitz", "bullet", "rapid"];
    Ok(PERF_TYPES
        .iter()
        .filter_map(|atype| user_performances[atype]["rating"].as_u64())
        .max())
}

/*
When running this bot, you first need to create a lichess bot account, then ask for an authorization
token for it. After these you can just run this executable and let it play. It will respond to
challenges (one at a time), also it will challenge bots currently available on lichess. By default
it does a 2.5 move deep full search.
 */
#[tokio::main]
async fn main() -> EmptyResult {
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
        format!("Bearer {authtoken}").parse().unwrap(),
    );
    let client = Client::builder().default_headers(headers).build().unwrap();
    let mut declining_bots = HashSet::new();

    let bots_best = best_rating_on_lichess(botid, &client)
        .await?
        .unwrap_or(1500);
    let bots_max_target = bots_best + 400;
    println!("This bot will not challenge better rated bots than {bots_max_target}");

    let mut gameid = None;
    if resume.to_lowercase().starts_with('y') {
        println!("Searching for previous, unfinished games..");
        let currentlyplaying =
            lichess_api_call(client.get("https://lichess.org/api/account/playing")).await?;
        let all_unfinished_games: Value =
            serde_json::from_str(currentlyplaying.text().await?.as_str())?;
        let agame = &all_unfinished_games["nowPlaying"][0];
        if agame.is_null() {
            println!("\tdid not find any.");
        } else {
            println!("Resuming game...");
            gameid = Some(String::from(agame["gameId"].as_str().unwrap()));
        }
    }
    loop {
        if let Some(gameid_str) = &gameid {
            println!("Starting to play game {gameid_str}");
            let result = play_a_game(gameid_str.as_str(), botid, &client).await?;
            // If we get a non-responsive opponent we ignore it from now on
            if let Some(problematicopponent) = result {
                declining_bots.insert(problematicopponent);
            }
            gameid = None;
        } else {
            println!("No game at the moment");
            if thread_rng().gen_bool(0.5) {
                let mut resp = lichess_api_call(client.get("https://lichess.org/api/bot/online"))
                    .await?
                    .bytes_stream();
                println!("Searching for bots: ");
                let mut bots = Vec::with_capacity(1000);
                while let Some(bytes) = resp.next().await {
                    for abot_json in String::from_utf8(Vec::from(bytes?.as_ref()))?.lines() {
                        if let Ok(abot) = serde_json::from_str(abot_json) {
                            let abot: Value = abot;
                            let botid = &abot["id"];
                            let botstr = botid.as_str().unwrap();
                            if !declining_bots.contains(botstr) {
                                bots.push(botstr.to_owned());
                            }
                        }
                    }
                }
                bots.retain(|b| !b.eq(botid));
                let target_bot = loop {
                    let target_bot_candidate =
                        bots.swap_remove(thread_rng().gen_range(0..bots.len()));
                    let best_rating =
                        best_rating_on_lichess(&target_bot_candidate, &client).await?;
                    if let Some(best) = best_rating {
                        if best > bots_max_target {
                            println!("Bot {target_bot_candidate} is too strong for us, its rating was found to be {best}");
                            sleep(Duration::from_millis(500));
                            continue;
                        }
                    }
                    break target_bot_candidate;
                };
                static CHALLENGE_TIME_CONTROLS: [(&str, &str); 10] = [
                    ("600", "10"),
                    ("600", "0"),
                    ("300", "5"),
                    ("300", "0"),
                    ("180", "3"),
                    ("180", "0"),
                    ("60", "1"),
                    ("60", "0"),
                    ("30", "1"),
                    ("30", "0"),
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
                    .post(format!("https://lichess.org/api/challenge/{target_bot}"))
                    .form(&req_form);
                println!("Challenging bot: {target_bot}, challenge details: {req_form:?}");
                let resp = lichess_api_call(post_req).await?;
                if !resp.status().is_success() {
                    let rsp_txt = resp.text().await?;
                    println!("Erroneous challenge response txt: {rsp_txt} ");
                }
            }
            println!("Waiting for events:");
            {
                let mut events =
                    lichess_api_call(client.get("https://lichess.org/api/stream/event"))
                        .await?
                        .bytes_stream();
                let startwait = Instant::now();
                'outer: while let Some(bytes) = events.next().await {
                    for eventupdate in String::from_utf8(Vec::from(bytes?.as_ref()))?.lines() {
                        if let Ok(event) = serde_json::from_str(eventupdate) {
                            let event: Value = event;
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
                                    println!("Declining challenge because: {reason}");
                                    let reasonmap = HashMap::from([("reason", reason)]);
                                    postreq = postreq.form(&reasonmap);
                                } else {
                                    println!(
                                        "Challange accepted from {}",
                                        event["challenger"]["id"].as_str().unwrap()
                                    );
                                }
                                lichess_api_call(postreq).await?;
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
