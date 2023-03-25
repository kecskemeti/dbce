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

use crate::baserules::board::Castling::{
    BlackKingSide, BlackQueenSide, WhiteKingSide, WhiteQueenSide,
};
use crate::baserules::board_rep::{BoardPos, PieceState, PossibleMove};
use crate::baserules::piece_color::PieceColor;
use crate::baserules::piece_color::PieceColor::{Black, White};
use crate::baserules::piece_kind::PieceKind::{Bishop, King, Knight, Pawn, Queen, Rook};
use ahash::AHashMap;
use enumset::{enum_set, EnumSet, EnumSetType};

pub static MATE: f32 = 1000.0;

pub static mut PSBCOUNT: u32 = 0;

#[derive(EnumSetType, Debug)]
pub enum Castling {
    WhiteKingSide,
    WhiteQueenSide,
    BlackKingSide,
    BlackQueenSide,
}

pub const fn white_can_castle() -> EnumSet<Castling> {
    enum_set!(WhiteKingSide | WhiteQueenSide)
}

pub const fn black_can_castle() -> EnumSet<Castling> {
    enum_set!(BlackKingSide | BlackQueenSide)
}

pub const fn queenside_castle() -> EnumSet<Castling> {
    enum_set!(BlackQueenSide | WhiteQueenSide)
}

pub const fn kingside_castle() -> EnumSet<Castling> {
    enum_set!(WhiteKingSide | BlackKingSide)
}

pub type RawBoard = [[Option<PieceState>; 8]; 8];

/// The internal representation of the chessboard after a given move.
#[derive(Clone)]
pub struct PSBoard {
    /// The actual board with the 8x8 squares
    pub board: RawBoard,
    /// Identifies the color who should move next
    pub who_moves: PieceColor,
    /// Tells what kind of castling is allowed
    pub castling: EnumSet<Castling>,
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

impl Default for PSBoard {
    /// Creates the standard starting position
    /// # Panics
    /// If we somehow end up generating a bigger than 8x8 board
    ///
    /// # Example
    /// ```
    /// use dbce::baserules::board::PSBoard;
    /// let starting_position = PSBoard::default();
    /// assert_eq!("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0", starting_position.to_fen());
    /// ```
    fn default() -> Self {
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
            castling: BlackKingSide | BlackQueenSide | WhiteKingSide | WhiteQueenSide,
            ep: None,
            move_count: 0,
            half_moves_since_pawn: 0,
            score: 0f32,
            continuation: AHashMap::new(),
        }
    }
}

impl PSBoard {
    /// Makes a move as per the internal representation
    /// Note that the move is not really checked for validity
    /// We will produce a completely new internal board representation as the result of the move
    /// This will allow further evaluation. This is done unless we have the board already pre-calculated earlier
    /// # Panics
    /// If there is a request to make a move for a piece that does not exist on the board
    ///
    /// # Example
    /// ```
    /// use dbce::baserules::board::PSBoard;
    /// use dbce::baserules::board_rep::{BaseMove, PossibleMove};
    /// let mut kings_knight_opening = PSBoard::from_fen("r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3");
    /// let bishop_moves = PossibleMove::simple_move(BaseMove::from_uci("f1b5").unwrap());
    /// let ruy_lopez = kings_knight_opening.make_a_move(&bishop_moves);
    /// assert_eq!("r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 3 3", ruy_lopez.to_fen());
    /// ```
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
                if let Some(ep) = &self.ep {
                    let current_piece = &self.get_loc(the_move.the_move.from).unwrap();
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
                castling: self.determine_castling_rights(current_piece, the_move, previous_piece),
                half_moves_since_pawn: self.half_moves_since_pawn + 1,
                move_count: self.move_count + u16::from(current_piece.color == Black),
                continuation: AHashMap::new(),
            }
        }
    }

    fn determine_castling_rights(
        &self,
        current_piece: &PieceState,
        the_move: &PossibleMove,
        possible_capture: &Option<PieceState>,
    ) -> EnumSet<Castling> {
        let mut new_castling = self.castling;
        if !self.castling.is_empty() {
            if current_piece.kind == King {
                new_castling ^= if current_piece.color == White {
                    WhiteKingSide | WhiteQueenSide
                } else {
                    BlackQueenSide | BlackKingSide
                };
            } else if current_piece.kind == Rook {
                if the_move.the_move.from.1 == 7 {
                    new_castling ^= if current_piece.color == White {
                        WhiteKingSide
                    } else {
                        BlackKingSide
                    };
                } else if the_move.the_move.from.1 == 0 {
                    new_castling ^= if current_piece.color == White {
                        WhiteQueenSide
                    } else {
                        BlackQueenSide
                    };
                }
            }
            if (the_move.the_move.to.0 == 0 || the_move.the_move.to.0 == 7)
                && (the_move.the_move.to.1 == 0 || the_move.the_move.to.1 == 7)
            {
                if let Some(taken) = possible_capture {
                    if taken.kind == Rook {
                        if the_move.the_move.to.1 == 0 {
                            new_castling ^= if current_piece.color == White {
                                BlackQueenSide
                            } else {
                                WhiteQueenSide
                            };
                        } else {
                            new_castling ^= if current_piece.color == White {
                                BlackKingSide
                            } else {
                                WhiteKingSide
                            };
                        }
                    }
                }
            }
        }
        new_castling
    }

    /// Determines what piece is at a particular location of the board
    ///
    /// # Example use:
    /// ```
    /// use dbce::baserules::board::PSBoard;
    /// use dbce::baserules::board_rep::PieceState;
    /// use dbce::baserules::piece_color::PieceColor::White;
    /// use dbce::baserules::piece_kind::PieceKind::King;
    ///
    /// let board = PSBoard::default();
    /// let king = board.get_loc((0,4));
    /// assert_eq!(&Some(PieceState { kind: King, color: White }),king)
    /// ```
    #[inline]
    pub fn get_loc(&self, (row, col): BoardPos) -> &Option<PieceState> {
        &self.board[row as usize][col as usize]
    }

    /// A simple scoring mechanism which just counts up the pieces and pawns based on their usual values
    ///
    /// # Example use
    /// Each `PSBoard` has its score automatically calculated with this method during creation, so this is an indirect demonstration.
    /// ```
    /// use dbce::baserules::board::{MATE, PSBoard};
    /// let scholars_mate = PSBoard::from_fen("1rbqQb1r/pppp2pp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b QKqk - 9 5");
    /// assert_eq!(MATE, scholars_mate.score);
    /// ```
    pub(crate) fn score_raw(board: &RawBoard) -> f32 {
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

#[cfg(test)]
mod test {
    use crate::baserules::board::Castling::{
        BlackKingSide, BlackQueenSide, WhiteKingSide, WhiteQueenSide,
    };
    use crate::baserules::board::{black_can_castle, queenside_castle, PSBoard};
    use crate::baserules::board_rep::{BaseMove, PieceState, PossibleMove};
    use crate::baserules::piece_kind::PieceKind::Queen;
    use enumset::enum_set;

    #[test]
    fn check_castling_when_rook_taken() {
        let silly_white =
            PSBoard::from_fen("rn1qkbnr/pbpppppp/1p6/8/5P2/3P2P1/PPP1P2P/RNBQKBNR b KQkq - 0 1");
        let moving_piece = PieceState::from_char('b');
        let captured_piece = Some(PieceState::from_char('R'));
        let the_capture = PossibleMove::simple_move(BaseMove::from_uci("b7h1").unwrap());
        let castling_result =
            silly_white.determine_castling_rights(&moving_piece, &the_capture, &captured_piece);
        assert_eq!(
            enum_set!(BlackQueenSide | BlackKingSide | WhiteQueenSide),
            castling_result
        );

        let crazy_white =
            PSBoard::from_fen("rnbqkbnr/pp1ppppp/8/8/4P1P1/3P1P1P/PpP5/RNBQKBNR b KQkq - 0 1");
        let moving_piece = PieceState::from_char('p');
        let captured_piece = Some(PieceState::from_char('R'));
        let the_capture = PossibleMove {
            the_move: BaseMove::from_uci("b2a1").unwrap(),
            pawn_promotion: Some(Queen),
            rook: None,
        };
        let castling_result =
            crazy_white.determine_castling_rights(&moving_piece, &the_capture, &captured_piece);
        assert_eq!(
            enum_set!(BlackQueenSide | BlackKingSide | WhiteKingSide),
            castling_result
        );
    }

    #[test]
    fn check_castling_when_rook_moves() {
        let casual_rook_black =
            PSBoard::from_fen("rnbqkbnr/1ppppppp/8/p7/3PP3/8/PPP2PPP/RNBQKBNR b KQkq - 0 1");
        let moving_piece = PieceState::from_char('r');
        let the_move = PossibleMove::simple_move(BaseMove::from_uci("a8a6").unwrap());
        let castling_result =
            casual_rook_black.determine_castling_rights(&moving_piece, &the_move, &None);
        assert_eq!(
            enum_set!(BlackKingSide | WhiteKingSide | WhiteQueenSide),
            castling_result
        );

        let attacking_rook_black =
            PSBoard::from_fen("rnbqkbnr/pppppp2/8/6P1/6p1/5P2/PPPPP3/RNBQKBNR b KQkq - 0 1");
        let moving_piece = PieceState::from_char('r');
        let captured_piece = Some(PieceState::from_char('R'));
        let the_move = PossibleMove::simple_move(BaseMove::from_uci("h8h1").unwrap());
        let castling_result = attacking_rook_black.determine_castling_rights(
            &moving_piece,
            &the_move,
            &captured_piece,
        );
        assert_eq!(queenside_castle(), castling_result);
    }

    #[test]
    fn check_castling_when_king_moves() {
        let bongcloud_opening_prep =
            PSBoard::from_fen("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1");
        let moving_piece = PieceState::from_char('K');
        let making_the_bongcloud = PossibleMove::simple_move(BaseMove::from_uci("e1e2").unwrap());

        let castling_result = bongcloud_opening_prep.determine_castling_rights(
            &moving_piece,
            &making_the_bongcloud,
            &None,
        );
        assert_eq!(black_can_castle(), castling_result);
    }
}
