/*
 *  ========================================================================
 *  DBCE chess bot, simple board representation structures
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

use crate::baserules::piece_color::PieceColor;
use crate::baserules::piece_color::PieceColor::*;
use crate::baserules::piece_kind::PieceKind;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::str::FromStr;

/// Used to represent the board position
/// For example: square a1 = (0,0), square h8 (7,7)
/// Also used to represent relative locations on the board (hence the signedness)
pub type BoardPos = (i8, i8);

/// Represents the pieces that can be placed on the board
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct PieceState {
    pub kind: PieceKind,
    pub color: PieceColor,
}

impl Display for PieceState {
    /// Turns piece state to FEN notation
    ///
    /// # Example:
    /// ```
    /// use dbce::baserules::board_rep::PieceState;
    /// use dbce::baserules::piece_color::PieceColor::White;
    /// use dbce::baserules::piece_kind::PieceKind::King;
    /// let white_king = PieceState {color: White, kind: King };
    /// assert_eq!("K", format!("{white_king}"))
    /// ```
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let uppercase_if_needed: fn(char) -> char = if self.color == White {
            |m| (m as u8 - (b'a' - b'A')) as char
        } else {
            |m| m
        };
        let res = uppercase_if_needed(self.kind.to_char());
        write!(f, "{res}")
    }
}

impl PieceState {
    /// Turns a single letter FEN notation to a piece state
    ///
    /// # Example:
    /// ```
    /// use dbce::baserules::board_rep::PieceState;
    /// use dbce::baserules::piece_color::PieceColor::White;
    /// use dbce::baserules::piece_kind::PieceKind::King;
    /// let white_king = PieceState {color: White, kind: King };
    /// assert_eq!(PieceState::from_char('K'),white_king);
    /// ```
    pub fn from_char(fen_piece: char) -> PieceState {
        PieceState {
            kind: PieceKind::from_char(fen_piece),
            color: if fen_piece.is_ascii_lowercase() {
                Black
            } else {
                White
            },
        }
    }
}

/// Simple move representation
#[derive(Eq, Hash, Copy, Clone, PartialEq, Debug)]
pub struct BaseMove {
    /// The square where the piece started to move from
    pub from: BoardPos,
    /// The square where the piece ended up on
    pub to: BoardPos,
}

impl BaseMove {
    pub fn from_two_pos(from: BoardPos, to: BoardPos) -> BaseMove {
        BaseMove { from, to }
    }

    /// Creates a base move from an uci string representation
    ///
    /// # Example
    /// ```
    /// use dbce::baserules::board_rep::BaseMove;
    /// let rook_a3 = BaseMove {
    ///     from: (0,0),
    ///     to: (2,0)
    /// };
    /// let potential_parsed_rook_move = BaseMove::from_uci("a1a3");
    /// assert!(potential_parsed_rook_move.is_ok());
    /// assert_eq!(rook_a3, potential_parsed_rook_move.unwrap());
    /// ```
    pub fn from_uci(uci: &str) -> Result<BaseMove, Box<dyn Error>> {
        Ok(BaseMove {
            from: (
                i8::from_str(&uci[1..2])? - 1,
                (uci.as_bytes()[0] - b'a') as i8,
            ),
            to: (
                i8::from_str(&uci[3..4])? - 1,
                (uci.as_bytes()[2] - b'a') as i8,
            ),
        })
    }
}

#[derive(Copy, Clone, Eq, Hash, PartialEq)]
pub struct PossibleMove {
    /// The move to do
    pub the_move: BaseMove,
    /// If the move was a pawn move and a promotion is necessary, then here we can indicate it
    pub pawn_promotion: Option<PieceKind>,
    /// If we have castling, then the rook also has to move alongside the king, the `BaseMove` is representing the king, here we represent the rook move
    pub rook: Option<BaseMove>,
}

impl PossibleMove {
    pub fn simple_move(base: BaseMove) -> PossibleMove {
        PossibleMove {
            the_move: base,
            ..Default::default()
        }
    }
}

impl Default for PossibleMove {
    fn default() -> Self {
        PossibleMove {
            the_move: BaseMove {
                from: (0, 0),
                to: (1, 0),
            },
            pawn_promotion: None,
            rook: None,
        }
    }
}

impl Display for PossibleMove {
    /// Produces moves in uci notation
    /// See also: <https://en.wikipedia.org/wiki/Universal_Chess_Interface>
    /// ```
    /// use dbce::baserules::board_rep::{BaseMove, PossibleMove};
    /// let rook_lift = PossibleMove {
    ///     the_move: BaseMove { from: (0,0), to: (2,0)},
    ///     pawn_promotion: None,
    ///     rook: None,
    /// };
    /// assert_eq!("a1a3",format!("{rook_lift}"));
    /// ```
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut ret = String::with_capacity(5);
        ret.push((self.the_move.from.1 as u8 + b'a') as char);
        ret.push((self.the_move.from.0 as u8 + b'1') as char);
        ret.push((self.the_move.to.1 as u8 + b'a') as char);
        ret.push((self.the_move.to.0 as u8 + b'1') as char);
        if let Some(pp) = &self.pawn_promotion {
            ret.push(pp.to_char());
        }
        write!(f, "{ret}",)
    }
}

impl Debug for PossibleMove {
    /// Mirrors `PossibleMove`'s display trait
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

#[cfg(test)]
mod test {
    use crate::baserules::board_rep::{BaseMove, PieceState, PossibleMove};
    use crate::baserules::piece_color::PieceColor::{Black, White};
    use crate::baserules::piece_kind::PieceKind::{Bishop, King, Knight, Pawn, Queen, Rook};

    #[test]
    fn pstest() {
        let triples = [
            (White, King, 'K'),
            (White, Knight, 'N'),
            (White, Bishop, 'B'),
            (White, Rook, 'R'),
            (White, Queen, 'Q'),
            (White, Pawn, 'P'),
            (Black, King, 'k'),
            (Black, Knight, 'n'),
            (Black, Bishop, 'b'),
            (Black, Rook, 'r'),
            (Black, Queen, 'q'),
            (Black, Pawn, 'p'),
        ];
        triples.iter().for_each(|(c, p, l)| {
            assert_eq!(
                format!(
                    "{}",
                    PieceState {
                        kind: *p,
                        color: *c
                    }
                ),
                format!("{l}")
            )
        });
    }

    #[test]
    fn ucitest() {
        let pawn_promote = PossibleMove {
            the_move: BaseMove {
                from: (1, 1),
                to: (0, 1),
            },
            pawn_promotion: Some(Queen),
            rook: None,
        };
        let black_short_castles = PossibleMove {
            the_move: BaseMove {
                from: (7, 3),
                to: (7, 1),
            },
            pawn_promotion: None,
            rook: Some(BaseMove {
                from: (7, 0),
                to: (7, 2),
            }),
        };
        let knight_moves = PossibleMove {
            the_move: BaseMove {
                from: (7, 6),
                to: (5, 5),
            },
            pawn_promotion: None,
            rook: None,
        };
        assert_eq!("b2b1q", format!("{pawn_promote}"));
        assert_eq!("d8b8", format!("{black_short_castles}"));
        assert_eq!("g8f6", format!("{knight_moves}"));
    }
}
