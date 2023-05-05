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

use crate::baserules::board_rep::PossibleMove;
use crate::baserules::piece_color::PieceColor;
use crate::engine::continuation::BoardContinuation;
use crate::engine::{gamestate::GameState, Engine};
use rayon::prelude::*;
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
    engine: &Engine,
    gamestate: &mut GameState,
    deadline: &Duration,
) -> (Duration, (Option<PossibleMove>, f32)) {
    println!("Set a deadline of: {deadline:?}");
    let ins = Instant::now();
    let machine_eval = engine.best_move_for(gamestate, deadline);
    let machine_move = machine_eval.0.as_ref().unwrap();
    let taken_this_much_time = ins.elapsed();
    let taken_this_much_time_ms = taken_this_much_time.as_millis();
    // visualise_explored_moves(gamestate.get_board());
    println!("Move took {} ms", taken_this_much_time_ms);
    println!("Went to depth {}", find_max_for_gs(gamestate));
    println!(
        "{} kNodes/sec",
        (machine_eval.2 as u128) / 1.max(taken_this_much_time_ms)
    );
    println!(
        "Evaluation result: {machine_move:?}, score: {}",
        machine_eval.1
    );
    (taken_this_much_time, (machine_eval.0, machine_eval.1))
}

pub fn find_max_for_gs(gs: &GameState) -> u8 {
    find_max_depth(gs.continuation())
}

pub fn find_max_depth(of_board: &BoardContinuation) -> u8 {
    internal_depth_par(of_board, 0)
}

fn internal_depth_seq(of_board: &BoardContinuation, curr_depth: u8) -> u8 {
    let next_depth = curr_depth + 1;
    of_board
        .continuation
        .iter()
        .map(|(_, next_board)| internal_depth_seq(next_board, next_depth))
        .max()
        .unwrap_or(next_depth)
}

fn internal_depth_par(of_board: &BoardContinuation, curr_depth: u8) -> u8 {
    let next_depth = curr_depth + 1;
    of_board
        .continuation
        .par_iter()
        .map(|(_, next_board)| internal_depth_seq(next_board, next_depth))
        .max()
        .unwrap_or(next_depth)
}

pub fn total_continuation_boards(of_board: &BoardContinuation) -> u32 {
    of_board.continuation.len() as u32
        + of_board
            .continuation
            .values()
            .map(total_continuation_boards)
            .sum::<u32>()
}

#[allow(dead_code)]
pub fn visualise_explored_moves(of_board: &BoardContinuation) {
    internal_visualise(of_board, 0);
}

fn internal_visualise(of_board: &BoardContinuation, depth: u8) {
    let prefix: String = (0..depth).map(|_| ' ').collect();
    let next_depth = depth + 1;
    for (a_move, its_board) in of_board.continuation.iter() {
        println!("{prefix}{a_move}");
        internal_visualise(its_board, next_depth);
    }
}
