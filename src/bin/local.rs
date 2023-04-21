/*
 *  ========================================================================
 *  DBCE chess bot, command line playable ui
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
use dbce::baserules::board::MATE;
use dbce::engine::{Engine, GameState};
use dbce::human_facing::helper::calculate_move_for_console;
use lazy_static::lazy_static;
use rand::{thread_rng, Rng};
use std::time::Duration;

lazy_static! {
    static ref ENGINE_THINK_TIME: Duration = Duration::from_secs(5);
}

fn make_machine_move(engine: &Engine, gamestate: &mut GameState) {
    println!("It's my move now, let me think:");
    let to_move = calculate_move_for_console(engine, gamestate, &ENGINE_THINK_TIME)
        .1
         .0
        .unwrap();
    gamestate.make_a_generated_move(&to_move);
}

fn main() {
    let input = std::io::stdin();
    println!("Custom starting position?");
    let mut line = String::new();
    {
        input.read_line(&mut line).unwrap();
    }
    let (engine, mut gamestate) = if line.trim().to_lowercase().starts_with('y') {
        println!("What is the FEN of the starting position?");
        let mut line = String::new();
        input.read_line(&mut line).unwrap();
        Engine::from_fen(&line)
    } else {
        Engine::new()
    };
    let machine_moves_first: bool = thread_rng().gen();
    if machine_moves_first {
        make_machine_move(&engine, &mut gamestate);
    }
    while (gamestate.get_board().score.abs() - MATE).abs() > 1.0 {
        println!("Current board: {}", gamestate.get_board());
        loop {
            println!("What's your move?");
            let mut line = String::new();
            {
                input.read_line(&mut line).unwrap();
                if gamestate.make_a_human_move(line.trim()).is_ok() {
                    break;
                }
            }
            println!("Sorry, I can't understand {line}");
        }
        println!("Current board: {}", gamestate.get_board());
        make_machine_move(&engine, &mut gamestate);
    }
}