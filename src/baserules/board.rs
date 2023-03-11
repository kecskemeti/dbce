/*
 *  ========================================================================
 *  DBCE chess bot, representation of a complete board and its possible future/past
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

extern crate rand;

use crate::baserules::board_rep::{BoardPos, PieceState, PossibleMove};
use crate::baserules::piece_color::PieceColor;
use crate::baserules::piece_color::PieceColor::*;
use crate::baserules::piece_kind::PieceKind::*;
use ahash::AHashMap;

pub static MATE: f32 = 1000.0;

pub static mut PSBCOUNT: u32 = 0;

pub type RawBoard = [[Option<PieceState>; 8]; 8];

/// The internal representation of the chessboard after a given move.
#[derive(Clone)]
pub struct PSBoard {
    /// The actual board with the 8x8 squares
    pub board: RawBoard,
    /// Identifies the color who should move next
    pub who_moves: PieceColor,
    /// Tells what kind of castling is allowed
    pub castling: u8,
    /// Tells if there is an en-passant move possible at the given location
    pub ep: Option<BoardPos>,
    /// The number of moves done so far
    pub move_count: u16,
    /// allows draw condition check
    pub half_moves_since_pawn: u16,
    /// The estimated score of this board
    pub score: f32,
    /// If we have calculated a few positions ahead from this board, we store these positions here
    pub continuation: AHashMap<PossibleMove, PSBoard>,
}

impl PSBoard {
    /// Creates the standard starting position
    pub fn new() -> PSBoard {
        let mut raw = [[None; 8]; 8];
        for (row_index, row) in raw.iter_mut().enumerate() {
            let (c, only_pawn) = match row_index {
                0 => (White, None),
                1 => (White, Some(Pawn)),
                6 => (Black, Some(Pawn)),
                7 => (Black, None),
                _ => continue,
            };
            for (column_index, a_square) in row.iter_mut().enumerate() {
                *a_square = Some(PieceState {
                    kind: if only_pawn.is_some() {
                        Pawn
                    } else {
                        match column_index {
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
            score: 0f32,
            continuation: AHashMap::new(),
        }
    }

    /// Makes a move as per the internal representation
    /// Note that the move is not really checked for validity
    /// We will produce a completely new internal board representation as the result of the move
    /// This will allow further evaluation. This is done unless we have the board already pre-calculated earlier
    pub fn make_a_move(&mut self, the_move: &PossibleMove) -> PSBoard {
        let the_new_board = self.continuation.remove(the_move);
        if let Some(precalculated_board) = the_new_board {
            // we have already pre-calculated the move before
            precalculated_board
        } else {
            // This is an unexpected move, or part of pre-calculation
            let mut raw_board = self.board;
            let previous_piece = self.get_loc(the_move.the_move.to);
            {
                let current_piece = raw_board[the_move.the_move.from.0 as usize]
                    [the_move.the_move.from.1 as usize]
                    .as_ref()
                    .unwrap();
                if let Some(ep) = &self.ep {
                    if current_piece.kind == Pawn
                        && ep.0 == the_move.the_move.to.0
                        && ep.1 == the_move.the_move.to.1
                    {
                        // En passant was done, the long move pawn was taken
                        raw_board[the_move.the_move.from.0 as usize]
                            [the_move.the_move.to.1 as usize] = None;
                    }
                }
            }
            // The move for almost all the cases
            raw_board[the_move.the_move.to.0 as usize][the_move.the_move.to.1 as usize] =
                raw_board[the_move.the_move.from.0 as usize][the_move.the_move.from.1 as usize];
            if let Some(promotion) = &the_move.pawn_promotion {
                //When we need to convert a pawn to something
                raw_board[the_move.the_move.to.0 as usize][the_move.the_move.to.1 as usize]
                    .as_mut()
                    .unwrap()
                    .kind = *promotion;
            } else if let Some(rook_move) = &the_move.rook {
                // when we are castling, the rook move is also stored
                raw_board[rook_move.to.0 as usize][rook_move.to.1 as usize] =
                    raw_board[rook_move.from.0 as usize][rook_move.from.1 as usize];
                raw_board[rook_move.from.0 as usize][rook_move.from.1 as usize] = None;
            }
            raw_board[the_move.the_move.from.0 as usize][the_move.the_move.from.1 as usize] = None; // Where we left from is now empty
            let current_piece = raw_board[the_move.the_move.to.0 as usize]
                [the_move.the_move.to.1 as usize]
                .as_ref()
                .unwrap();
            PSBoard {
                score: PSBoard::score_raw(&raw_board),
                board: raw_board,
                who_moves: match current_piece.color {
                    White => Black,
                    Black => White,
                },
                ep: if current_piece.kind == Pawn
                    && (the_move.the_move.from.0 - the_move.the_move.to.0).abs() == 2
                {
                    Some((
                        (the_move.the_move.from.0 + the_move.the_move.to.0) >> 1,
                        the_move.the_move.to.1,
                    ))
                } else {
                    None
                },
                castling: self.castling & {
                    if current_piece.kind == King {
                        if current_piece.color == White {
                            3
                        } else {
                            12
                        }
                    } else if current_piece.kind == Rook {
                        if the_move.the_move.from.1 == 0 {
                            if current_piece.color == White {
                                7
                            } else {
                                13
                            }
                        } else if the_move.the_move.from.1 == 7 {
                            if current_piece.color == White {
                                11
                            } else {
                                14
                            }
                        } else {
                            15
                        }
                    } else if (the_move.the_move.to.0 == 0 || the_move.the_move.to.0 == 7)
                        && (the_move.the_move.to.1 == 0 || the_move.the_move.to.1 == 7)
                    {
                        if let Some(taken) = previous_piece {
                            if taken.kind == Rook {
                                let mut rook_side =
                                    if the_move.the_move.to.1 == 7 { 1u8 } else { 2 };
                                rook_side <<= if the_move.the_move.to.0 == 0 { 2 } else { 0 };
                                (!rook_side) & 15
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
                move_count: self.move_count + if current_piece.color == Black { 1 } else { 0 },
                continuation: AHashMap::new(),
            }
        }
    }

    /// Determines what piece is at a particular location of the board
    #[inline]
    pub fn get_loc(&self, (row, col): BoardPos) -> &Option<PieceState> {
        &self.board[row as usize][col as usize]
    }

    /// A simple scoring mechanism which just counts up the pieces and pawns based on their usual values
    fn score_raw(board: &RawBoard) -> f32 {
        let (loc_score, white_king_found, black_king_found) = board
            .iter()
            .flatten()
            .flatten()
            .map(|curr_piece| match (curr_piece.kind, curr_piece.color) {
                (Pawn, White) => (1f32, false, false),
                (Pawn, Black) => (-1f32, false, false),
                (Knight, White) => (3f32, false, false),
                (Knight, Black) => (-3f32, false, false),
                (Bishop, White) => (3.1f32, false, false),
                (Bishop, Black) => (-3.1f32, false, false),
                (Rook, White) => (5f32, false, false),
                (Rook, Black) => (-5f32, false, false),
                (Queen, White) => (9f32, false, false),
                (Queen, Black) => (-9f32, false, false),
                (King, White) => (0f32, true, false),
                (King, Black) => (0f32, false, true),
            })
            .fold(
                (0f32, false, false),
                |(curr_score, curr_white_king, curr_black_king),
                 (score_adjust, white_king, black_king)| {
                    (
                        curr_score + score_adjust,
                        curr_white_king | white_king,
                        curr_black_king | black_king,
                    )
                },
            );
        if white_king_found {
            if black_king_found {
                loc_score
            } else {
                MATE
            }
        } else {
            -MATE
        }
    }
}
