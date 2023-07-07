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
use crate::baserules::positions::AbsoluteBoardPos;
use crate::util::{AnyError, IntResult, TryWithPanic};
use std::fmt::{Debug, Display, Formatter};

/// Simple move representation
#[derive(Eq, Hash, Copy, Clone, PartialEq, Debug)]
pub struct BaseMove {
    /// The square where the piece started to move from
    pub from: AbsoluteBoardPos,
    /// The square where the piece ended up on
    pub to: AbsoluteBoardPos,
}

impl BaseMove {
    pub fn from_two_pos(from: AbsoluteBoardPos, to: AbsoluteBoardPos) -> Self {
        BaseMove { from, to }
    }

    pub fn move_in_row(row: u8, from_col: u8, to_col: u8) -> Self {
        BaseMove::from_two_pos((row, from_col).transform(), (row, to_col).transform())
    }

    /// Creates a base move from an uci string representation
    ///
    /// # Example
    /// ```
    /// use dbce::baserules::board_rep::BaseMove;
    /// use dbce::util::TryWithPanic;
    /// let rook_a3 = BaseMove {
    ///     from: "a1".transform(),
    ///     to: "a3".transform()
    /// };
    /// let potential_parsed_rook_move = BaseMove::from_uci("a1a3");
    /// assert!(potential_parsed_rook_move.is_ok());
    /// assert_eq!(rook_a3, potential_parsed_rook_move.unwrap());
    /// ```
    pub fn from_uci(uci: &str) -> IntResult<Self> {
        uci.try_into()
    }
}

impl TryFrom<&str> for BaseMove {
    type Error = AnyError;

    fn try_from(uci: &str) -> IntResult<Self> {
        Ok(Self {
            from: uci[0..2].try_into()?,
            to: uci[2..4].try_into()?,
        })
    }
}

impl Default for BaseMove {
    fn default() -> Self {
        BaseMove {
            from: AbsoluteBoardPos::default(),
            to: (0, 1).transform(),
        }
    }
}

impl Display for BaseMove {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.from, self.to)
    }
}

#[derive(Copy, Clone, Eq, Hash, PartialEq, Default, Debug)]
pub struct PossibleMove {
    /// The move to do
    pub the_move: BaseMove,
    /// If the move was a pawn move and a promotion is necessary, then here we can indicate it
    pub pawn_promotion: Option<PieceKind>,
    /// If we have castling, then the rook also has to move alongside the king, the `BaseMove` is representing the king, here we represent the rook move
    pub rook: Option<BaseMove>,
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

impl Display for PossibleMove {
    /// Produces moves in uci notation
    /// See also: <https://en.wikipedia.org/wiki/Universal_Chess_Interface>
    /// ```
    /// use dbce::baserules::board_rep::{BaseMove, PossibleMove};
    /// use dbce::util::TryWithPanic;
    /// let rook_lift = PossibleMove {
    ///     the_move: BaseMove { from: "a1".transform(), to: "a3".transform()},
    ///     pawn_promotion: None,
    ///     rook: None,
    /// };
    /// assert_eq!("a1a3",format!("{rook_lift}"));
    /// ```
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            self.the_move,
            self.pawn_promotion
                .map(|pp| format!("{}", pp.to_char()))
                .unwrap_or("".into())
        )
    }
}

#[cfg(test)]
mod test {
    use crate::baserules::board_rep::{BaseMove, PossibleMove};
    use crate::baserules::piece_color::PieceColor::{Black, White};
    use crate::baserules::piece_kind::PieceKind::{Bishop, King, Knight, Pawn, Queen, Rook};
    use crate::baserules::piece_state::PieceState;
    use crate::baserules::positions::AbsoluteBoardPos;

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
            the_move: BaseMove::move_in_row(7, 3, 1),
            pawn_promotion: None,
            rook: Some(BaseMove::move_in_row(7, 0, 2)),
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
