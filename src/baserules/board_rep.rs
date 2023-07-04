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
use crate::util::{AnyError, IntResult};
use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter};
use std::ops;
use std::str::FromStr;

/// Used to represent the board position
/// For example: square a1 = (0,0), square h8 (7,7)
/// Also used to represent relative locations on the board (hence the signedness)
#[derive(Eq, Hash, Copy, Clone, PartialEq, Debug)]
pub struct RelativeBoardPos(pub i8, pub i8);

impl TryFrom<(i8, i8)> for RelativeBoardPos {
    type Error = AnyError;
    /// Allows loading absolute board coordinates to a BoardPos struct
    fn try_from((row, col): (i8, i8)) -> Result<Self, Self::Error> {
        if (-8..8).contains(&row) && (-8..8).contains(&col) {
            Ok(RelativeBoardPos(row, col))
        } else {
            Err(format!("Row and column index out of range for {row},{col}").into())
        }
    }
}

#[derive(Eq, Hash, Copy, Clone, PartialEq, Debug)]
pub struct AbsoluteBoardPos(pub u8, pub u8);

impl TryFrom<&str> for AbsoluteBoardPos {
    type Error = AnyError;
    /// Allows loading absolute board coordinates to a BoardPos struct
    fn try_from(coord: &str) -> Result<Self, Self::Error> {
        let row = u8::from_str(&coord[1..2])? - 1;
        let col: u8 = (coord.as_bytes()[0] - b'a').try_into()?;
        (row, col).try_into()
    }
}

impl TryFrom<(u8, u8)> for AbsoluteBoardPos {
    type Error = AnyError;
    /// Allows loading absolute board coordinates to a BoardPos struct
    fn try_from((row, col): (u8, u8)) -> Result<Self, Self::Error> {
        if (0..8).contains(&row) && (0..8).contains(&col) {
            Ok(AbsoluteBoardPos(row, col))
        } else {
            Err(format!("Row and column index out of range for {row},{col}").into())
        }
    }
}

impl ops::Add<RelativeBoardPos> for AbsoluteBoardPos {
    type Output = AbsoluteBoardPos;

    fn add(self, rhs: RelativeBoardPos) -> Self::Output {
        self.fallible_add(rhs).unwrap()
    }
}

impl AbsoluteBoardPos {
    pub fn fallible_add(self, rhs: RelativeBoardPos) -> IntResult<AbsoluteBoardPos> {
        let row = self.0 as i8 + rhs.0;
        let col = self.1 as i8 + rhs.1;
        (row.try_into()?, col.try_into()?).try_into()?
    }
}

impl ops::AddAssign<RelativeBoardPos> for AbsoluteBoardPos {
    fn add_assign(&mut self, rhs: RelativeBoardPos) {
        self.0 = (self.0 as i8 + rhs.0) as u8;
        self.1 = (self.1 as i8 + rhs.1) as u8;
    }
}

pub trait TryWithPanic<T> {
    fn transform(self) -> T;
}

impl<AbsoluteBoardPos> TryWithPanic<AbsoluteBoardPos> for (u8, u8) {
    fn transform(self) -> AbsoluteBoardPos {
        self.try_into().unwrap()
    }
}

impl<RelativeBoardPos> TryWithPanic<RelativeBoardPos> for (i8, i8) {
    fn transform(self) -> RelativeBoardPos {
        self.try_into().unwrap()
    }
}

impl RelativeBoardPos {
    pub fn transform_vec(in_vec: Vec<(i8, i8)>) -> Vec<RelativeBoardPos> {
        in_vec
            .into_iter()
            .map(|tuple| tuple.try_into().unwrap())
            .collect()
    }
}

/// Simple move representation
#[derive(Eq, Hash, Copy, Clone, PartialEq, Debug)]
pub struct BaseMove {
    /// The square where the piece started to move from
    pub from: AbsoluteBoardPos,
    /// The square where the piece ended up on
    pub to: AbsoluteBoardPos,
}

impl BaseMove {
    /// Allows easy comparison of moves by compacting the move into a single 32 bit integer
    fn to_u32(self) -> u32 {
        (self.from.0 as u32) << 24
            | (self.from.1 as u32) << 16
            | (self.to.0 as u32) << 8
            | self.to.1 as u32
    }

    pub fn from_two_pos(from: AbsoluteBoardPos, to: AbsoluteBoardPos) -> Self {
        BaseMove { from, to }
    }

    /// Creates a base move from an uci string representation
    ///
    /// # Example
    /// ```
    /// use dbce::baserules::board_rep::{BaseMove, BoardPos};
    /// let rook_a3 = BaseMove {
    ///     from: "a1".try_into().unwrap(),
    ///     to: "a3".try_into().unwrap()
    /// };
    /// let potential_parsed_rook_move = BaseMove::from_uci("a1a3");
    /// assert!(potential_parsed_rook_move.is_ok());
    /// assert_eq!(rook_a3, potential_parsed_rook_move.unwrap());
    /// ```
    pub fn from_uci(uci: &str) -> IntResult<Self> {
        Ok(Self {
            from: uci[0..2].try_into()?,
            to: uci[2..4].try_into()?,
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
    pub fn simple_move(base: BaseMove) -> Self {
        Self {
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
    pub fn simple_from_uci(uci: &str) -> IntResult<PossibleMove> {
        Ok(PossibleMove::simple_move(BaseMove::from_uci(uci)?))
    }
}

impl Default for PossibleMove {
    fn default() -> Self {
        Self {
            the_move: BaseMove {
                from: AbsoluteBoardPos(0, 0),
                to: AbsoluteBoardPos(1, 0),
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
    ///     the_move: BaseMove { from: "a1".try_into().unwrap(), to: "a3".try_into().unwrap()},
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
    use crate::baserules::board_rep::{AbsoluteBoardPos, BaseMove, PossibleMove};
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
    fn clone_eq_test() {
        let pm = PossibleMove::simple_from_uci("h4h5").unwrap();
        let copy_pm = pm;
        let pm_ref = &pm;
        let pm_clone_ref = &copy_pm;
        assert_eq!(pm_ref, pm_clone_ref);
    }

    #[test]
    fn ucitest() {
        let pawn_promote = PossibleMove {
            the_move: BaseMove {
                from: AbsoluteBoardPos(1, 1),
                to: AbsoluteBoardPos(0, 1),
            },
            pawn_promotion: Some(Queen),
            rook: None,
        };
        let black_short_castles = PossibleMove {
            the_move: BaseMove {
                from: AbsoluteBoardPos(7, 3),
                to: AbsoluteBoardPos(7, 1),
            },
            pawn_promotion: None,
            rook: Some(BaseMove {
                from: AbsoluteBoardPos(7, 0),
                to: AbsoluteBoardPos(7, 2),
            }),
        };
        let knight_moves = PossibleMove {
            the_move: BaseMove {
                from: AbsoluteBoardPos(7, 6),
                to: AbsoluteBoardPos(5, 5),
            },
            pawn_promotion: None,
            rook: None,
        };
        assert_eq!("b2b1q", format!("{pawn_promote}"));
        assert_eq!("d8b8", format!("{black_short_castles}"));
        assert_eq!("g8f6", format!("{knight_moves}"));
    }
}
