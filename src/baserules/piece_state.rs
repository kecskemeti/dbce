/*
 *  ========================================================================
 *  DBCE chess bot, rules of chess piece representation
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
use crate::baserules::piece_color::PieceColor::{Black, White};
use crate::baserules::piece_kind::PieceKind;
use crate::baserules::piece_kind::PieceKind::{Bishop, King, Knight, Pawn, Queen, Rook};
use lazy_static::lazy_static;
use rustc_hash::FxHashMap;
use std::fmt::{Debug, Display, Formatter};

/// Represents the pieces that can be placed on the board
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct PieceState {
    pub kind: PieceKind,
    pub color: PieceColor,
}

impl Display for PieceState {
    /// Turns piece state to FEN notation
    ///
    /// # Example:
    /// ```
    /// use dbce::baserules::piece_color::PieceColor::White;
    /// use dbce::baserules::piece_kind::PieceKind::King;
    /// use dbce::baserules::piece_state::PieceState;
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

static ALL_POSSIBLE_PIECE_STATES: [Option<PieceState>; 15] = [
    None,
    Some(PieceState {
        kind: King,
        color: White,
    }),
    Some(PieceState {
        kind: Queen,
        color: White,
    }),
    Some(PieceState {
        kind: Bishop,
        color: White,
    }),
    Some(PieceState {
        kind: Knight,
        color: White,
    }),
    Some(PieceState {
        kind: Rook,
        color: White,
    }),
    Some(PieceState {
        kind: Pawn,
        color: White,
    }),
    None,
    None,
    Some(PieceState {
        kind: King,
        color: Black,
    }),
    Some(PieceState {
        kind: Queen,
        color: Black,
    }),
    Some(PieceState {
        kind: Bishop,
        color: Black,
    }),
    Some(PieceState {
        kind: Knight,
        color: Black,
    }),
    Some(PieceState {
        kind: Rook,
        color: Black,
    }),
    Some(PieceState {
        kind: Pawn,
        color: Black,
    }),
];

lazy_static! {
    static ref REVERSE_POSSIBLE_PIECE_STATES: FxHashMap<&'static Option<PieceState>, usize> =
        ALL_POSSIBLE_PIECE_STATES
            .iter()
            .enumerate()
            .map(|(idx, ps)| (ps, idx))
            .collect();
}

impl PieceState {
    /// Turns a single letter FEN notation to a piece state
    ///
    /// # Example:
    /// ```
    /// use dbce::baserules::piece_state::PieceState;
    /// use dbce::baserules::piece_color::PieceColor::White;
    /// use dbce::baserules::piece_kind::PieceKind::King;
    /// let white_king = PieceState {color: White, kind: King };
    /// assert_eq!(PieceState::from_char('K'),white_king);
    /// ```
    pub fn from_char(fen_piece: char) -> Self {
        PieceState {
            kind: PieceKind::from_char(fen_piece),
            color: if fen_piece.is_ascii_lowercase() {
                Black
            } else {
                White
            },
        }
    }

    pub fn pawn_promote(&self, kind: PieceKind) -> &'static Option<PieceState> {
        assert_eq!(self.kind, Pawn);
        let new_state = PieceState { kind, ..*self };
        &ALL_POSSIBLE_PIECE_STATES[*(REVERSE_POSSIBLE_PIECE_STATES.get(&Some(new_state)).unwrap())]
    }

    pub fn from_u8(bit_repr: u8) -> &'static Option<Self> {
        &ALL_POSSIBLE_PIECE_STATES[bit_repr as usize]
    }

    pub fn bits(a_piece: &Option<PieceState>) -> u8 {
        *REVERSE_POSSIBLE_PIECE_STATES.get(&a_piece).unwrap() as u8
    }
}

#[cfg(test)]
mod test {
    use crate::baserules::piece_color::PieceColor::{Black, White};
    use crate::baserules::piece_kind::PieceKind::{King, Queen};
    use crate::baserules::piece_state::PieceState;

    #[test]
    fn lookup() {
        assert_eq!(
            Some(PieceState {
                kind: King,
                color: White
            }),
            *PieceState::from_u8(1)
        );
        assert_eq!(
            Some(PieceState {
                kind: King,
                color: Black
            }),
            *PieceState::from_u8(1 + 8)
        );
    }
    #[test]
    fn promotion() {
        let a_pawn = PieceState::from_u8(6);
        let a_promoted_pawn = a_pawn.unwrap().pawn_promote(Queen);
        assert_eq!(
            Some(PieceState {
                kind: Queen,
                color: White
            }),
            *a_promoted_pawn
        );
        let a_pawn = PieceState::from_u8(6 + 8);
        let a_promoted_pawn = a_pawn.unwrap().pawn_promote(Queen);
        assert_eq!(
            Some(PieceState {
                kind: Queen,
                color: Black
            }),
            *a_promoted_pawn
        );
    }
    #[test]
    fn conversion() {
        let white_king = Some(PieceState {
            kind: King,
            color: White,
        });
        assert_eq!(1, PieceState::bits(&white_king));
        let black_king = Some(PieceState {
            kind: King,
            color: Black,
        });
        assert_eq!(1 + 8, PieceState::bits(&black_king));
    }
}
