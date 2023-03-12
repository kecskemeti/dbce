/*
 *  ========================================================================
 *  DBCE chess bot, core engine
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
use crate::baserules::board::{PSBoard, MATE, PSBCOUNT};
use crate::baserules::board_rep::PossibleMove;
use crate::baserules::piece_color::PieceColor::*;
use crate::human_facing::moves::make_an_uci_move;
use crate::util::{DurationAverage, VecCache};
use rand::{thread_rng, Rng};
use std::cmp::Ordering;
use std::ptr;
use std::time::Duration;
use tokio::time::Instant;

pub struct Engine {
    move_cache: VecCache<PossibleMove>,
    scoring_timings: DurationAverage,
}

pub struct GameState {
    worked_on_board: PSBoard,
}

impl GameState {
    pub fn get_board(&self) -> &PSBoard {
        &self.worked_on_board
    }

    pub fn make_an_uci_move(&mut self, themove: &str) {
        self.worked_on_board = make_an_uci_move(&mut self.worked_on_board, themove);
    }
}

impl Engine {
    pub fn new() -> (Engine, GameState) {
        let mut sample_board = PSBoard::new();
        let mut moves = Vec::new();
        sample_board.gen_potential_moves(false, &mut moves);
        let mut scoring_timings = DurationAverage::new(50, || Duration::from_secs(1));
        for mv in 0..scoring_timings.len as usize {
            scoring_timings.add(Engine::timed_move(&mut sample_board, &moves[mv % moves.len()]).1);
        }
        (
            Engine {
                move_cache: VecCache::new(),
                scoring_timings,
            },
            GameState {
                worked_on_board: PSBoard::new(),
            },
        )
    }

    fn timed_move(board: &mut PSBoard, amove: &PossibleMove) -> (PSBoard, Duration) {
        let pre = Instant::now();
        (board.make_a_move(amove), pre.elapsed())
    }

    fn timing_remembering_move(&mut self, board: &mut PSBoard, amove: &PossibleMove) -> PSBoard {
        let (ret, dur) = Engine::timed_move(board, amove);
        self.scoring_timings.add(dur);
        ret
    }

    fn similar_quality_moves<'a>(
        start_board: &'a PSBoard,
        best_board: &'a PSBoard,
    ) -> impl Iterator<Item = &'a PSBoard> {
        start_board.continuation.values().filter(|other| {
            (other.score.max(best_board.score) - other.score.min(best_board.score)) < 0.001
        })
    }

    pub fn best_move_for(
        &mut self,
        state: &mut GameState,
        deadline: &Duration,
    ) -> (Option<PossibleMove>, f32) {
        self.best_move_for_internal(&mut state.worked_on_board, deadline)
    }

    // Determines the best move on the depth asked for
    // If the decide flag is passed, we will have the move generated, otherwise we just use this method for scoring
    fn best_move_for_internal(
        &mut self,
        start_board: &mut PSBoard,
        deadline: &Duration,
    ) -> (Option<PossibleMove>, f32) {
        let mut ret = (None, start_board.score);
        let mate_multiplier = match start_board.who_moves {
            White => 1.0,
            Black => -1.0,
        };
        if (start_board.score.abs() - MATE).abs() > 0.1 {
            let mut moves = self.move_cache.get();
            assert_eq!(0, moves.len());
            start_board.gen_potential_moves(true, &mut moves);
            let movecount = moves.len();
            //            println!("{} Potential moves: {:?}", prefix, moves);
            let single_move_deadline = deadline
                .checked_div(movecount as u32)
                .expect("Could not generate a single move!!!");
            // println!("Single move deadline: {:?}", single_move_deadline);
            while let Some(curr_move) = moves.pop() {
                let mut board_with_move = self.timing_remembering_move(start_board, &curr_move);
                unsafe {
                    PSBCOUNT += 1;
                }
                if (board_with_move.score - mate_multiplier * MATE).abs() < 0.1 {
                    // No need to search further we have a mate
                    ret = (Some(curr_move), board_with_move.score);
                    break;
                }
                let average_scoring_duration = self.scoring_timings.calc_average() * 16;
                if average_scoring_duration < single_move_deadline {
                    board_with_move.score = (self
                        .best_move_for_internal(&mut board_with_move, &single_move_deadline)
                        .1
                        * 10.0
                        + start_board.score
                        + board_with_move.score)
                        / 12.0;
                };
                start_board.continuation.insert(curr_move, board_with_move);
            }
            if ret.0.is_none() {
                // we don't have a mate
                let best_potential_board = start_board.continuation.values().max_by(|b1, b2| {
                    (mate_multiplier * b1.score)
                        .partial_cmp(&(mate_multiplier * b2.score))
                        .unwrap_or(Ordering::Less)
                });
                if let Some(best_board) = best_potential_board {
                    let choices = Engine::similar_quality_moves(start_board, best_board).count();
                    let selected_board = Engine::similar_quality_moves(start_board, best_board)
                        .nth(thread_rng().gen_range(0..choices))
                        .unwrap();
                    ret = start_board
                        .continuation
                        .iter()
                        .find_map(|(amove, aboard)| {
                            if ptr::eq(aboard, selected_board) {
                                Some((Some(*amove), selected_board.score))
                            } else {
                                None
                            }
                        })
                        .unwrap();
                }
            }
            self.move_cache.release(moves);
        }
        ret
    }
}
