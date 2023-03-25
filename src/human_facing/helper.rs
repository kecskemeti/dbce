/*
 *  ========================================================================
 *  DBCE chess bot, bot-ui interaction
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

use crate::baserules::board::{PSBoard, PSBCOUNT};
use crate::baserules::board_rep::PossibleMove;
use crate::baserules::piece_color::PieceColor;
use crate::engine::{Engine, GameState};
use std::sync::Mutex;
use std::time::Duration;
use tokio::time::Instant;

impl PieceColor {
    pub fn is_this_resignable(&self, score: f32) -> bool {
        match self {
            PieceColor::White => score < -100.0,
            PieceColor::Black => score > 100.0,
        }
    }
}

pub fn calculate_move_for_console(
    engine: &mut Engine,
    gamestate: &mut GameState,
    deadline: &Duration,
) -> (Duration, (Option<PossibleMove>, f32)) {
    println!("Set a deadline of: {deadline:?}");
    let ins = Instant::now();
    unsafe {
        PSBCOUNT = 0;
    }
    let machine_eval = engine.best_move_for(gamestate, deadline);
    let machine_move = machine_eval.0.as_ref().unwrap();
    let taken_this_much_time = ins.elapsed();
    let taken_this_much_time_ms = taken_this_much_time.as_millis();
    // visualise_explored_moves(gamestate.get_board());
    println!("Went to depth {}", find_max_depth(gamestate.get_board()));
    println!("Move took {} ms", taken_this_much_time_ms);
    unsafe {
        println!(
            "{} kNodes/sec",
            u128::from(PSBCOUNT) / 1.max(taken_this_much_time_ms)
        );
    }
    println!(
        "Evaluation result: {machine_move:?}, score: {}",
        machine_eval.1
    );
    (taken_this_much_time, machine_eval)
}

pub fn find_max_depth(of_board: &PSBoard) -> u8 {
    let max = Mutex::new(0);
    internal_depth(of_board, 0, &max);
    let final_max = max.lock().unwrap();
    *final_max
}

fn internal_depth(of_board: &PSBoard, curr_depth: u8, max: &Mutex<u8>) {
    {
        let mut int_mutex = max.lock().unwrap();
        if curr_depth > *int_mutex {
            *int_mutex = curr_depth;
        }
    }
    let next_depth = curr_depth + 1;
    for (_, next) in of_board.continuation.iter() {
        internal_depth(next, next_depth, max);
    }
}

#[allow(dead_code)]
pub fn visualise_explored_moves(of_board: &PSBoard) {
    internal_visualise(of_board, 0);
}

fn internal_visualise(of_board: &PSBoard, depth: u8) {
    let prefix: String = (0..depth).map(|_| ' ').collect();
    let next_depth = depth + 1;
    for (a_move, its_board) in of_board.continuation.iter() {
        println!("{prefix}{a_move}");
        internal_visualise(its_board, next_depth);
    }
}
