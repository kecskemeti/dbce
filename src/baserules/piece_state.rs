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
    static ref ALL_POSSIBLE_PIECE_STATES_SPARSE: FxHashMap<u32, &'static Option<PieceState>> =
        ALL_POSSIBLE_PIECE_STATES
            .iter()
            .enumerate()
            .flat_map(shift)
            .collect();
    static ref ALL_POSSIBLE_MASKS: [u32; (u32::BITS / 4) as usize] = generate_masks();
}

fn shift(
    (idx, ps): (usize, &'static Option<PieceState>),
) -> impl Iterator<Item = (u32, &'static Option<PieceState>)> {
    (0..u32::BITS / 4).map(move |shift_amount| ((idx as u32) << (shift_amount * 4), ps))
}

fn generate_masks() -> [u32; (u32::BITS / 4) as usize] {
    let mut mask_permutations = [0; (u32::BITS / 4) as usize];

    for (index, shift) in (0..u32::BITS).step_by(4).enumerate() {
        let n: u32 = 0xF << shift;
        mask_permutations[index] = n;
    }

    mask_permutations
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
        Self {
            kind: PieceKind::from_char(fen_piece),
            color: if fen_piece.is_ascii_lowercase() {
                Black
            } else {
                White
            },
        }
    }

    #[inline]
    pub fn pawn_promote(&self, kind: PieceKind) -> &'static Option<Self> {
        assert_eq!(self.kind, Pawn);
        let new_state = Self { kind, ..*self };
        Self::from_u32(Self::bits_u32(&Some(new_state)))
    }

    #[inline]
    pub fn from_u32(bit_repr: u32) -> &'static Option<Self> {
        ALL_POSSIBLE_PIECE_STATES_SPARSE[&bit_repr]
    }

    #[inline]
    pub fn masked_ps_conversion(col: usize, unmasked: u32) -> &'static Option<Self> {
        PieceState::from_u32(unsafe { ALL_POSSIBLE_MASKS.get_unchecked(col) } & unmasked)
    }

    #[inline]
    pub fn bits(a_piece: &Option<PieceState>) -> u8 {
        Self::bits_usize(a_piece) as u8
    }

    #[inline]
    pub fn bits_u32(a_piece: &Option<PieceState>) -> u32 {
        Self::bits_usize(a_piece) as u32
    }

    #[inline]
    pub fn bits_usize(a_piece: &Option<PieceState>) -> usize {
        REVERSE_POSSIBLE_PIECE_STATES[a_piece]
    }
}

#[cfg(test)]
mod test {
    use crate::baserules::piece_color::PieceColor::{Black, White};
    use crate::baserules::piece_kind::PieceKind::{King, Queen};
    use crate::baserules::piece_state::{generate_masks, PieceState};

    #[test]
    fn lookup() {
        assert_eq!(
            Some(PieceState {
                kind: King,
                color: White
            }),
            *PieceState::from_u32(1)
        );
        assert_eq!(
            Some(PieceState {
                kind: King,
                color: Black
            }),
            *PieceState::from_u32(1 + 8)
        );
    }
    #[test]
    fn promotion() {
        let a_pawn = PieceState::from_u32(6);
        let a_promoted_pawn = a_pawn.unwrap().pawn_promote(Queen);
        assert_eq!(
            Some(PieceState {
                kind: Queen,
                color: White
            }),
            *a_promoted_pawn
        );
        let a_pawn = PieceState::from_u32(6 + 8);
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

    #[test]
    pub fn test_mask_generator() {
        // [0b1111, 0b11110000, 0b111100000000, 0b1111000000000000, ...];
        let masks = generate_masks();
        assert_eq!(masks[0], 0b1111);
        assert_eq!(masks[1], 0b11110000);
        assert_eq!(masks[2], 0b111100000000);
        assert_eq!(masks[3], 0b1111000000000000);
        assert_eq!(masks[7], 0b11110000000000000000000000000000);
    }
}
