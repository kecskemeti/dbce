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

use crate::baserules::piece_kind::PieceKind;
use std::cmp::Ordering;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::str::FromStr;

/// Used to represent the board position
/// For example: square a1 = (0,0), square h8 (7,7)
/// Also used to represent relative locations on the board (hence the signedness)
#[derive(Eq, Hash, Copy, Clone, PartialEq, Debug)]
pub struct BoardPos(pub i8, pub i8);

impl FromStr for BoardPos {
    type Err = Box<dyn Error>;

    fn from_str(coord: &str) -> Result<Self, Self::Err> {
        Ok(BoardPos(
            i8::from_str(&coord[1..2])? - 1,
            (coord.as_bytes()[0] - b'a') as i8,
        ))
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
    fn to_u32(self) -> u32 {
        (self.from.0 as u32) << 24
            | (self.from.1 as u32) << 16
            | (self.to.0 as u32) << 8
            | self.to.1 as u32
    }

    pub fn from_two_pos(from: BoardPos, to: BoardPos) -> Self {
        BaseMove { from, to }
    }

    /// Creates a base move from an uci string representation
    ///
    /// # Example
    /// ```
    /// use dbce::baserules::board_rep::{BaseMove, BoardPos};
    /// let rook_a3 = BaseMove {
    ///     from: BoardPos(0,0),
    ///     to: BoardPos(2,0)
    /// };
    /// let potential_parsed_rook_move = BaseMove::from_uci("a1a3");
    /// assert!(potential_parsed_rook_move.is_ok());
    /// assert_eq!(rook_a3, potential_parsed_rook_move.unwrap());
    /// ```
    pub fn from_uci(uci: &str) -> Result<Self, Box<dyn Error>> {
        Ok(Self {
            from: BoardPos::from_str(&uci[0..2])?,
            to: BoardPos::from_str(&uci[2..4])?,
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

impl PartialOrd<Self> for PossibleMove {
    fn partial_cmp(&self, _: &Self) -> Option<Ordering> {
        todo!()
    }
}

impl Ord for PossibleMove {
    fn cmp(&self, other: &Self) -> Ordering {
        self.the_move.to_u32().cmp(&other.the_move.to_u32())
    }

    fn max(self, _: Self) -> Self {
        todo!()
    }

    fn min(self, _: Self) -> Self {
        todo!()
    }

    fn clamp(self, _: Self, _: Self) -> Self {
        todo!()
    }
}

impl PossibleMove {
    /// Allows creating moves that does not involve pawn promotions or castling with a simple call
    ///
    /// # Example
    /// ```
    /// use dbce::baserules::board_rep::{BaseMove, PossibleMove};
    /// let uci = "e5e6";
    /// let a_move = PossibleMove::simple_move(BaseMove::from_uci(uci).unwrap());
    /// assert_eq!(uci, format!("{a_move}"));
    /// ```
    pub fn simple_move(base: BaseMove) -> PossibleMove {
        PossibleMove {
            the_move: base,
            ..Default::default()
        }
    }

    /// Allows creating moves that does not involve pawn promotions or castling when an uci representation is available
    ///
    /// # Example
    /// ```
    /// use dbce::baserules::board_rep::{BaseMove, PossibleMove};
    /// let uci = "e5e6";
    /// let a_move = PossibleMove::simple_from_uci(uci).unwrap();
    /// assert_eq!(uci, format!("{a_move}"));
    /// ```
    pub fn simple_from_uci(uci: &str) -> Result<PossibleMove, Box<dyn Error>> {
        Ok(PossibleMove::simple_move(BaseMove::from_uci(uci)?))
    }
}

impl Default for PossibleMove {
    fn default() -> Self {
        PossibleMove {
            the_move: BaseMove {
                from: BoardPos(0, 0),
                to: BoardPos(1, 0),
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
    /// use dbce::baserules::board_rep::{BaseMove, BoardPos, PossibleMove};
    /// let rook_lift = PossibleMove {
    ///     the_move: BaseMove { from: BoardPos(0,0), to: BoardPos(2,0)},
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
    use crate::baserules::board_rep::{BaseMove, BoardPos, PossibleMove};
    use crate::baserules::piece_color::PieceColor::{Black, White};
    use crate::baserules::piece_kind::PieceKind::{Bishop, King, Knight, Pawn, Queen, Rook};
    use crate::baserules::piece_state::PieceState;

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
                from: BoardPos(1, 1),
                to: BoardPos(0, 1),
            },
            pawn_promotion: Some(Queen),
            rook: None,
        };
        let black_short_castles = PossibleMove {
            the_move: BaseMove {
                from: BoardPos(7, 3),
                to: BoardPos(7, 1),
            },
            pawn_promotion: None,
            rook: Some(BaseMove {
                from: BoardPos(7, 0),
                to: BoardPos(7, 2),
            }),
        };
        let knight_moves = PossibleMove {
            the_move: BaseMove {
                from: BoardPos(7, 6),
                to: BoardPos(5, 5),
            },
            pawn_promotion: None,
            rook: None,
        };
        assert_eq!("b2b1q", format!("{pawn_promote}"));
        assert_eq!("d8b8", format!("{black_short_castles}"));
        assert_eq!("g8f6", format!("{knight_moves}"));
    }
}
