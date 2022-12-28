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
 *  (C) Copyright 2022, Gabor Kecskemeti
 */

extern crate rand;

use std::borrow::BorrowMut;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use tokio::time::Instant;

use crate::board_rep::PieceColor::*;
use crate::board_rep::PieceKind::*;
use crate::board_rep::{BaseMove, PieceColor, PieceKind, PieceState, PossibleMove};
use crate::util::DurationAverage;

use self::rand::{thread_rng, Rng};

static MATE: f32 = 1000.0;

pub static mut PSBCOUNT: u32 = 0;

/*
The internal representation of the chessboard after a given move.
*/
#[derive(Copy, Clone)]
pub struct PSBoard {
    pub board: [[Option<PieceState>; 8]; 8],
    pub who_moves: PieceColor,
    pub castling: u8,
    pub ep: Option<(i8, i8)>,
    pub move_count: u16,
    pub half_moves_since_pawn: u16,
}

impl PSBoard {
    // Creates the standard starting position
    pub fn new() -> PSBoard {
        let mut raw = [[None; 8]; 8];
        for (ridx, row) in raw.iter_mut().enumerate() {
            let (c, onlypawn) = match ridx {
                0 => (White, None),
                1 => (White, Some(Pawn)),
                6 => (Black, Some(Pawn)),
                7 => (Black, None),
                _ => continue,
            };
            for (cidx, asquare) in row.iter_mut().enumerate() {
                *asquare = Some(PieceState {
                    kind: if onlypawn.is_some() {
                        Pawn
                    } else {
                        match cidx {
                            0 | 7 => Rook,
                            1 | 6 => Knight,
                            2 | 5 => Bishop,
                            3 => Queen,
                            4 => King,
                            _ => panic!("Impossible"),
                        }
                    },
                    color: c,
                });
            }
        }
        PSBoard {
            board: raw,
            who_moves: White,
            castling: 15,
            ep: None,
            move_count: 0,
            half_moves_since_pawn: 0,
        }
    }

    // Allows moves to be translated from lichess to our internal representation
    pub fn make_an_uci_move(&self, themove: &str) -> PSBoard {
        let len = themove.len();
        assert!(len < 6 && len > 3);
        let rawmove = themove.as_bytes();
        let from = ((rawmove[1] - b'1') as i8, (rawmove[0] - b'a') as i8);
        let to = ((rawmove[3] - b'1') as i8, (rawmove[2] - b'a') as i8);
        if len == 5 {
            let promotekind = PieceKind::from_char(rawmove[4] as char);
            self.make_a_move(&PossibleMove {
                themove: BaseMove { from, to },
                pawnpromotion: Some(promotekind),
                rook: None,
            })
        } else {
            let movingpiece = self.get_loc(&from).as_ref().unwrap();
            let rookfrom = (from.0, if to.1 == 6 { 7 } else { 0 });
            let rookto = (from.0, if to.1 == 6 { 5 } else { 3 });
            self.make_a_move(&PossibleMove {
                themove: BaseMove { from, to },
                pawnpromotion: None,
                rook: if self.castling != 0
                    && (to.0 == 0 || to.0 == 7)
                    && from.1 == 4
                    && (to.1 == 6 || to.1 == 2)
                    && movingpiece.kind == King
                {
                    Some(BaseMove {
                        from: rookfrom,
                        to: rookto,
                    })
                } else {
                    None
                },
            })
        }
    }

    // Makes a move as per the internal representation
    // Note that the move is not really checked for validity
    // We will produce a completely new internal board representation as the result of the move
    // This will allow further evaluation
    pub fn make_a_move(&self, themove: &PossibleMove) -> PSBoard {
        let mut raw = self.board;
        let prevpiece = self.get_loc(&themove.themove.to);
        {
            let currpiece = raw[themove.themove.from.0 as usize][themove.themove.from.1 as usize]
                .as_ref()
                .unwrap();
            if let Some(ep) = &self.ep {
                if currpiece.kind == Pawn
                    && ep.0 == themove.themove.to.0
                    && ep.1 == themove.themove.to.1
                {
                    // En passant was done, the long move pawn was taken
                    raw[themove.themove.from.0 as usize][themove.themove.to.1 as usize] = None;
                }
            }
        }
        // The move for almost all the cases
        raw[themove.themove.to.0 as usize][themove.themove.to.1 as usize] =
            raw[themove.themove.from.0 as usize][themove.themove.from.1 as usize];
        if let Some(promotion) = &themove.pawnpromotion {
            //When we need to convert a pawn to something
            raw[themove.themove.to.0 as usize][themove.themove.to.1 as usize]
                .as_mut()
                .unwrap()
                .kind = *promotion;
        } else if let Some(rookmove) = &themove.rook {
            // when we are castling, the rook move is also stored
            raw[rookmove.to.0 as usize][rookmove.to.1 as usize] =
                raw[rookmove.from.0 as usize][rookmove.from.1 as usize];
            raw[rookmove.from.0 as usize][rookmove.from.1 as usize] = None;
        }
        raw[themove.themove.from.0 as usize][themove.themove.from.1 as usize] = None; // Where we left from is now empty
        let currpiece = raw[themove.themove.to.0 as usize][themove.themove.to.1 as usize]
            .as_ref()
            .unwrap();
        PSBoard {
            board: raw,
            who_moves: match currpiece.color {
                White => Black,
                Black => White,
            },
            ep: if currpiece.kind == Pawn
                && ((themove.themove.from.0 as i8) - (themove.themove.to.0 as i8)).abs() == 2
            {
                Some((
                    (themove.themove.from.0 + themove.themove.to.0) >> 1,
                    themove.themove.to.1,
                ))
            } else {
                None
            },
            castling: self.castling & {
                if currpiece.kind == King {
                    if currpiece.color == White {
                        3
                    } else {
                        12
                    }
                } else if currpiece.kind == Rook {
                    if themove.themove.from.1 == 0 {
                        if currpiece.color == White {
                            7
                        } else {
                            13
                        }
                    } else if themove.themove.from.1 == 7 {
                        if currpiece.color == White {
                            11
                        } else {
                            14
                        }
                    } else {
                        15
                    }
                } else if (themove.themove.to.0 == 0 || themove.themove.to.0 == 7)
                    && (themove.themove.to.1 == 0 || themove.themove.to.1 == 7)
                {
                    if let Some(taken) = prevpiece {
                        if taken.kind == Rook {
                            let mut rookside = if themove.themove.to.1 == 7 { 1u8 } else { 2 };
                            rookside <<= if themove.themove.to.0 == 0 { 2 } else { 0 };
                            (!rookside) & 15
                        } else {
                            15
                        }
                    } else {
                        15
                    }
                } else {
                    15
                }
            },
            half_moves_since_pawn: self.half_moves_since_pawn + 1,
            move_count: self.move_count + if currpiece.color == Black { 1 } else { 0 },
        }
    }

    // Determines what piece is at a particular location of the board
    #[inline]
    pub fn get_loc(&self, (row, col): &(i8, i8)) -> &Option<PieceState> {
        &self.board[*row as usize][*col as usize]
    }

    // A simple scoring mechanism which just counts up the pieces and pawns based on their usual values
    pub fn score(&self) -> f32 {
        let mut loc_score = 0f32;
        let mut white_king_found = false;
        let mut black_king_found = false;
        for row in 0..8usize {
            for col in 0..8usize {
                if let Some(currpiece) = &self.board[row][col] {
                    let mult = if currpiece.color == White {
                        1f32
                    } else {
                        -1f32
                    };
                    loc_score += mult
                        * match currpiece.kind {
                            Pawn => 1f32,
                            Knight => 3f32,
                            Bishop => 3.1f32,
                            Rook => 5f32,
                            Queen => 9f32,
                            King => {
                                if currpiece.color == White {
                                    white_king_found = true;
                                } else {
                                    black_king_found = true;
                                }
                                0f32
                            }
                        }
                }
            }
        }
        if white_king_found {
            if black_king_found {
                loc_score as f32
            } else {
                MATE
            }
        } else {
            -MATE
        }
    }
}

pub struct Engine {
    vec_cache: Arc<Mutex<Vec<Vec<(PossibleMove, f32)>>>>,
    scoring_timings: DurationAverage,
}

impl Engine {
    pub fn new() -> Engine {
        let sample_board = PSBoard::new();
        Engine {
            vec_cache: Arc::new(Mutex::new(Vec::new())),
            scoring_timings: DurationAverage::new(50, || Engine::timed_score(&sample_board).1),
        }
    }

    fn timed_score(board: &PSBoard) -> (f32, Duration) {
        let pre = Instant::now();
        (board.score(), pre.elapsed())
    }

    fn timing_remembering_score(&mut self, board: &PSBoard) -> f32 {
        let (ret, dur) = Engine::timed_score(board);
        self.scoring_timings.add(dur);
        ret
    }

    fn get_cached(&mut self) -> Vec<(PossibleMove, f32)> {
        self.vec_cache
            .borrow_mut()
            .lock()
            .unwrap()
            .pop()
            .unwrap_or_else(|| Vec::new())
    }

    fn release_cached(&mut self, mut scored_moves: Vec<(PossibleMove, f32)>) {
        scored_moves.clear();
        self.vec_cache
            .borrow_mut()
            .lock()
            .unwrap()
            .push(scored_moves)
    }

    // Determines the best move on the depth asked for
    // If the decide flag is passed, we will have the move generated, otherwise we just use this method for scoring
    pub fn best_move_for(
        &mut self,
        start_board: &PSBoard,
        decide: bool,
        deadline: &Duration,
    ) -> (Option<PossibleMove>, f32) {
        let sc = self.timing_remembering_score(start_board);
        let mut ret = (None, sc);
        if (sc.abs() - MATE).abs() > 0.1 {
            let mut scored_moves = self.get_cached();
            assert_eq!(0, scored_moves.len());
            start_board.gen_potential_moves(true, &mut scored_moves);
            let movecount = scored_moves.len();
            //            println!("{} Potential moves: {:?}", prefix, moves);
            let single_move_deadline = deadline
                .checked_div(movecount as u32)
                .expect("Could not generate a single move!!!");
            // println!("Single move deadline: {:?}", single_move_deadline);
            for (curr_move, score) in &mut scored_moves {
                //                    println!("{}  Testing move {:?}", prefix, curr_move);
                let board_with_move = start_board.make_a_move(&curr_move);
                unsafe {
                    PSBCOUNT += 1;
                }
                let average_scoring_duration = self.scoring_timings.calc_average() * 16;
                // println!(
                //     "Expected minimum time for scoring: {:?}",
                //     average_scoring_duration
                // );
                let curr_score = if average_scoring_duration > single_move_deadline {
                    self.timing_remembering_score(&board_with_move)
                } else {
                    (self
                        .best_move_for(&board_with_move, false, &single_move_deadline)
                        .1
                        * 10.0
                        + sc)
                        / 11.0
                };
                // println!("Acquired score: {}", curr_score);
                *score = curr_score;
                if start_board.who_moves == White {
                    if (curr_score - MATE).abs() < 0.1 {
                        // No need to search further we have a mate
                        break;
                    }
                } else if (curr_score + MATE).abs() < 0.1 {
                    // No need to search further we have a mate
                    break;
                }
            }
            let sorting = match start_board.who_moves {
                White => 1.0,
                Black => -1.0,
            };
            let lastscore = scored_moves.len() - 1;
            scored_moves.sort_unstable_by(|(_, s1), (_, s2)| {
                (sorting * s1).partial_cmp(&(s2 * sorting)).unwrap() // UNWRAP CAN FAIL HERE!
            });
            if (scored_moves[lastscore].1.abs() - MATE).abs() < 0.1 {
                // End of game
                let best = scored_moves.swap_remove(lastscore);
                ret = (if decide { Some(best.0) } else { None }, best.1);
            } else {
                let best = &scored_moves[lastscore];
                let sameasbest = if (best.1 - scored_moves[0].1).abs() < 0.001 {
                    lastscore
                } else {
                    let closetoup = scored_moves.binary_search_by(|(_, s)| {
                        (sorting * s)
                            .partial_cmp(&((best.1 + 0.001) * sorting))
                            .unwrap()
                    });
                    let closetodown = scored_moves.binary_search_by(|(_, s)| {
                        (sorting * s)
                            .partial_cmp(&((best.1 - 0.001) * sorting))
                            .unwrap()
                    });
                    let upidx = match closetoup {
                        Ok(loc) => loc,
                        Err(loc) => loc,
                    };
                    let downidx = match closetodown {
                        Ok(loc) => loc,
                        Err(loc) => loc,
                    };
                    upidx.max(downidx) - upidx.min(downidx)
                };

                let best = if sameasbest == 0 {
                    scored_moves.pop().unwrap()
                } else {
                    let mut rng = thread_rng();
                    let idx = rng.gen_range(0..sameasbest);
                    scored_moves.swap_remove(lastscore - idx)
                };
                ret = (Some(best.0), best.1)
            }
            self.release_cached(scored_moves);
        }
        ret
    }
}
