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
use crate::baserules::board_rep::PossibleMove;
use crate::baserules::piece_color::PieceColor;
use crate::baserules::piece_color::PieceColor::{Black, White};
use crate::baserules::piece_kind::PieceKind::{King, Pawn, Rook};
use crate::baserules::piece_state::PieceState;
use crate::baserules::rawboard::RawBoard;

use enumset::{enum_set, EnumSet, EnumSetType};

use super::board_rep::AbsoluteBoardPos;
use super::move_gen::{KingMove, CASTLE_ALLOWED, CASTLE_FORBIDDEN};

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

/// The internal representation of the chessboard after a given move.
pub struct PSBoard {
    /// The actual board with the 8x8 squares
    pub raw: RawBoard,
    /// Identifies the color who should move next
    pub who_moves: PieceColor,
    /// Tells what kind of castling is allowed
    pub castling: EnumSet<Castling>,
    /// move resolver
    pub(crate) king_move_gen: &'static dyn KingMove,
    /// Tells if there is an en-passant move possible at the given location
    pub ep: Option<AbsoluteBoardPos>,
    /// The number of moves done so far
    pub move_count: u16,
    /// allows draw condition check
    pub half_moves_since_pawn: u16,
    /// The estimated score of this board, without considering its possible continuations
    pub score: f32,
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
        PSBoard {
            raw: RawBoard::default(),
            who_moves: White,
            castling: BlackKingSide | BlackQueenSide | WhiteKingSide | WhiteQueenSide,
            ep: None,
            move_count: 0,
            half_moves_since_pawn: 0,
            score: 0f32,
            king_move_gen: &CASTLE_ALLOWED,
        }
    }
}

impl PSBoard {
    /// Makes a move as per the internal representation
    /// Note that the move is not really checked for validity
    /// We will produce a completely new internal board representation as the result of the move
    /// This will allow further evaluation.
    /// # Panics
    /// If there is a request to make a move for a piece that does not exist on the board
    ///
    /// # Example
    /// ```
    /// use dbce::baserules::board::PSBoard;
    /// use dbce::baserules::board_rep::{BaseMove, PossibleMove};
    /// let mut kings_knight_opening = PSBoard::from_fen("r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3");
    /// let bishop_moves = PossibleMove::simple_from_uci("f1b5").unwrap();
    /// let ruy_lopez = kings_knight_opening.make_move_noncached(&bishop_moves);
    /// assert_eq!("r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 3 3", ruy_lopez.to_fen());
    /// ```
    pub fn make_move_noncached(&self, the_move: &PossibleMove) -> Self {
        let mut raw_board = self.raw;
        let piece_potentially_taken = self.get_loc(the_move.the_move.to);
        {
            if let Some(ep) = &self.ep {
                let current_piece = &self.get_loc(the_move.the_move.from).unwrap();
                if current_piece.kind == Pawn
                    && ep.0 == the_move.the_move.to.0
                    && ep.1 == the_move.the_move.to.1
                {
                    // En passant was done, the long move pawn was taken
                    raw_board.set_loc(
                        (the_move.the_move.from.0, the_move.the_move.to.1).into(),
                        &None,
                    );
                }
            }
        }
        // The move for almost all the cases
        let piece_before_move = self.get_loc(the_move.the_move.from);
        raw_board.set_loc(the_move.the_move.to, piece_before_move);
        if let Some(promotion) = &the_move.pawn_promotion {
            //When we need to convert a pawn to something
            let piece = raw_board[the_move.the_move.to]
                .unwrap()
                .pawn_promote(*promotion);
            raw_board.set_loc(the_move.the_move.to, piece);
        } else if let Some(rook_move) = &the_move.rook {
            // when we are castling, the rook move is also stored
            raw_board.set_loc(rook_move.to, self.get_loc(rook_move.from));
            raw_board.set_loc(rook_move.from, &None);
        }
        raw_board.set_loc(the_move.the_move.from, &None); // Where we left from is now empty
        let current_piece = raw_board[the_move.the_move.to].unwrap();
        let (castling, king_move_gen) =
            self.determine_castling_rights(&current_piece, the_move, piece_potentially_taken);
        PSBoard {
            score: raw_board.score(),
            raw: raw_board,
            who_moves: match current_piece.color {
                White => Black,
                Black => White,
            },
            ep: if current_piece.kind == Pawn
                && (the_move.the_move.from.0 as i8 - the_move.the_move.to.0 as i8).abs() == 2
            {
                Some(
                    (
                        (the_move.the_move.from.0 + the_move.the_move.to.0) >> 1,
                        the_move.the_move.to.1,
                    )
                        .try_into()
                        .unwrap(),
                )
            } else {
                None
            },
            castling,
            king_move_gen,
            half_moves_since_pawn: if let Pawn = piece_before_move.unwrap().kind {
                0
            } else if piece_potentially_taken.is_some() {
                0
            } else {
                self.half_moves_since_pawn + 1
            },
            move_count: self.move_count + u16::from(current_piece.color == Black),
        }
    }

    fn determine_castling_rights(
        &self,
        current_piece: &PieceState,
        the_move: &PossibleMove,
        possible_capture: &Option<PieceState>,
    ) -> (EnumSet<Castling>, &'static dyn KingMove) {
        let mut new_castling = self.castling;
        let mut king_mover = self.king_move_gen;
        if !self.castling.is_empty() {
            let mut changed = false;
            if current_piece.kind == King {
                changed = true;
                new_castling ^= if current_piece.color == White {
                    WhiteKingSide | WhiteQueenSide
                } else {
                    BlackQueenSide | BlackKingSide
                };
            } else if current_piece.kind == Rook {
                if the_move.the_move.from.1 == 7 {
                    changed = true;
                    new_castling ^= if current_piece.color == White {
                        WhiteKingSide
                    } else {
                        BlackKingSide
                    };
                } else if the_move.the_move.from.1 == 0 {
                    changed = true;
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
                            changed = true;
                            new_castling ^= if current_piece.color == White {
                                BlackQueenSide
                            } else {
                                WhiteQueenSide
                            };
                        } else {
                            changed = true;
                            new_castling ^= if current_piece.color == White {
                                BlackKingSide
                            } else {
                                WhiteKingSide
                            };
                        }
                    }
                }
            }
            if changed {
                king_mover = if new_castling.is_empty() {
                    &CASTLE_FORBIDDEN
                } else {
                    &CASTLE_ALLOWED
                };
            }
        }
        (new_castling, king_mover)
    }

    /// Determines what piece is at a particular location of the board
    ///
    /// # Example use:
    /// ```
    /// use dbce::baserules::board::PSBoard;
    /// use dbce::baserules::board_rep::BoardPos;
    /// use dbce::baserules::piece_color::PieceColor::White;
    /// use dbce::baserules::piece_kind::PieceKind::King;
    /// use dbce::baserules::piece_state::PieceState;
    ///
    /// let board = PSBoard::default();
    /// let king = board.get_loc("e1".try_into().unwrap());
    /// assert_eq!(&Some(PieceState { kind: King, color: White }),king)
    /// ```
    #[inline]
    pub fn get_loc(&self, pos: AbsoluteBoardPos) -> &Option<PieceState> {
        &self.raw[pos]
    }
}

#[cfg(test)]
mod test {
    use crate::baserules::board::Castling::{
        BlackKingSide, BlackQueenSide, WhiteKingSide, WhiteQueenSide,
    };
    use crate::baserules::board::{black_can_castle, queenside_castle, PSBoard};
    use crate::baserules::board_rep::{BaseMove, PossibleMove};
    use crate::baserules::piece_color::PieceColor::White;
    use crate::baserules::piece_kind::PieceKind::{King, Queen, Rook};
    use crate::baserules::piece_state::PieceState;
    use enumset::enum_set;

    #[test]
    fn check_castling_when_rook_taken() {
        let silly_white =
            PSBoard::from_fen("rn1qkbnr/pbpppppp/1p6/8/5P2/3P2P1/PPP1P2P/RNBQKBNR b KQkq - 0 1");
        let moving_piece = PieceState::from_char('b');
        let captured_piece = Some(PieceState::from_char('R'));
        let the_capture = PossibleMove::simple_from_uci("b7h1").unwrap();
        let (castling_result, _) =
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
        let (castling_result, _) =
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
        let the_move = PossibleMove::simple_from_uci("a8a6").unwrap();
        let (castling_result, _) =
            casual_rook_black.determine_castling_rights(&moving_piece, &the_move, &None);
        assert_eq!(
            enum_set!(BlackKingSide | WhiteKingSide | WhiteQueenSide),
            castling_result
        );

        let attacking_rook_black =
            PSBoard::from_fen("rnbqkbnr/pppppp2/8/6P1/6p1/5P2/PPPPP3/RNBQKBNR b KQkq - 0 1");
        let moving_piece = PieceState::from_char('r');
        let captured_piece = Some(PieceState::from_char('R'));
        let the_move = PossibleMove::simple_from_uci("h8h1").unwrap();
        let (castling_result, _) = attacking_rook_black.determine_castling_rights(
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
        let making_the_bongcloud = PossibleMove::simple_from_uci("e1e2").unwrap();

        let (castling_result, _) = bongcloud_opening_prep.determine_castling_rights(
            &moving_piece,
            &making_the_bongcloud,
            &None,
        );
        assert_eq!(black_can_castle(), castling_result);
    }

    #[test]
    fn move_castling() {
        let prep_for_castle =
            PSBoard::from_fen("rnbqk2r/pppp1ppp/3bpn2/8/8/3BPN2/PPPP1PPP/RNBQK2R w KQkq - 4 4");
        let after_move = prep_for_castle.make_move_noncached(&PossibleMove {
            the_move: BaseMove::from_uci("e1g1").unwrap(),
            pawn_promotion: None,
            rook: Some(BaseMove::from_uci("h1f1").unwrap()),
        });
        assert_eq!(
            PieceState {
                kind: King,
                color: White
            },
            after_move.get_loc("g1".try_into().unwrap()).unwrap()
        );
        assert_eq!(
            PieceState {
                kind: Rook,
                color: White
            },
            after_move.get_loc("f1".try_into().unwrap()).unwrap()
        );
    }
}
