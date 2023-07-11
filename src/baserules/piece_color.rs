/*
 *  ========================================================================
 *  DBCE chess bot, piece colour specific details
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
use crate::baserules::castling::Castling::{
    BlackKingSide, BlackQueenSide, WhiteKingSide, WhiteQueenSide,
};
use crate::baserules::castling::{black_can_castle, white_can_castle, Castling};
use crate::baserules::positions::{AbsoluteBoardPos, RelativeBoardPos};
use crate::util::{AnyError, IntResult};
use enum_map::{enum_map, Enum, EnumMap};
use enumset::EnumSet;
use lazy_static::lazy_static;
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use PieceColor::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Enum, Hash)]
pub enum PieceColor {
    Black,
    White,
}

lazy_static! {
    /// Colour dependent directional pawn moves for pawns that have already taken their first move
    static ref PAWN_SINGLE_STEPS: EnumMap<PieceColor, Vec<RelativeBoardPos>> = enum_map! {
        Black => RelativeBoardPos::transform_to_vec([(-1, 0)]),
        White => RelativeBoardPos::transform_to_vec([(1, 0)])
    };
    /// Colour dependent directional pawn moves for pawns that have not moved yet
    static ref PAWN_DOUBLE_STEPS: EnumMap<PieceColor, Vec<RelativeBoardPos>> = enum_map! {
        Black => RelativeBoardPos::transform_to_vec([(-1, 0), (-2, 0)]),
        White => RelativeBoardPos::transform_to_vec([(1, 0), (2, 0)])
    };
    /// Colour dependent directional pawn moves for pawns that can take opponent pieces
    static ref PAWN_TAKES_STEPS: EnumMap<PieceColor, Vec<RelativeBoardPos>> = enum_map! {
        Black => RelativeBoardPos::transform_to_vec([(-1, 1), (-1, -1)]),
        White => RelativeBoardPos::transform_to_vec([(1, 1), (1, -1)])
    };
}

impl PieceColor {
    /// Quick query for regular pawn move direction per colour
    #[inline]
    pub fn pawn_single_step(self) -> &'static Vec<RelativeBoardPos> {
        &PAWN_SINGLE_STEPS[self]
    }
    /// Quick query for the first pawn move direction per colour
    #[inline]
    pub fn pawn_double_step(self) -> &'static Vec<RelativeBoardPos> {
        &PAWN_DOUBLE_STEPS[self]
    }
    /// Quick query for the taking pawn moves per colour
    #[inline]
    pub fn pawn_takes_step(self) -> &'static Vec<RelativeBoardPos> {
        &PAWN_TAKES_STEPS[self]
    }
    /// Determine pawn promotion row for a colour
    #[inline]
    pub const fn pawn_promotion_row(self) -> u8 {
        match self {
            Black => 0,
            White => 7,
        }
    }
    /// Determine piece starting row for a colour
    #[inline]
    pub const fn piece_row(self) -> u8 {
        match self {
            Black => 7,
            White => 0,
        }
    }
    #[inline]
    pub const fn pawn_starting_row(self) -> u8 {
        match self {
            Black => 6,
            White => 1,
        }
    }
    #[inline]
    pub fn from_u8(colour: u8) -> Self {
        if colour & 8 > 0 {
            White
        } else {
            Black
        }
    }
    #[inline]
    pub fn add_to_u8(self, prepped: u8) -> u8 {
        prepped
            | match self {
                White => 8,
                Black => 0,
            }
    }
    #[inline]
    pub const fn all_castling(&self) -> EnumSet<Castling> {
        match self {
            Black => black_can_castle(),
            White => white_can_castle(),
        }
    }
    #[inline]
    pub const fn king_side_castling(&self) -> Castling {
        match self {
            Black => BlackKingSide,
            White => WhiteKingSide,
        }
    }
    #[inline]
    pub const fn queen_side_castling(&self) -> Castling {
        match self {
            Black => BlackQueenSide,
            White => WhiteQueenSide,
        }
    }
    #[inline]
    pub const fn invert(&self) -> Self {
        match self {
            White => Black,
            Black => White,
        }
    }
    #[inline]
    pub const fn starting_king_pos(&self) -> AbsoluteBoardPos {
        AbsoluteBoardPos(self.piece_row(), 4)
    }
    #[inline]
    pub const fn fen_color(&self) -> char {
        match self {
            White => 'w',
            Black => 'b',
        }
    }
    #[inline]
    pub const fn mate_multiplier(&self) -> f32 {
        match self {
            White => 1.0,
            Black => -1.0,
        }
    }

    #[inline]
    pub const fn score_comparator(&self) -> impl FnMut(&f32, &f32) -> Ordering {
        match self {
            White => |a: &f32, b: &f32| a.partial_cmp(b).unwrap(),
            Black => |a: &f32, b: &f32| b.partial_cmp(a).unwrap(),
        }
    }

    #[inline]
    pub fn is_better_score(&self, good: f32, candidate: f32) -> bool {
        match self {
            White => good < candidate,
            Black => good > candidate,
        }
    }

    pub const fn worst_score(&self) -> f32 {
        match self {
            White => f32::NEG_INFINITY,
            Black => f32::INFINITY,
        }
    }
}

impl Display for PieceColor {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fen_color())
    }
}

impl TryFrom<char> for PieceColor {
    type Error = AnyError;

    fn try_from(value: char) -> IntResult<Self> {
        match value {
            'w' => Ok(White),
            'b' => Ok(Black),
            _ => Err("Invalid piece color char".into()),
        }
    }
}
