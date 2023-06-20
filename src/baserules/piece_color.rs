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
use crate::baserules::board_rep::BoardPos;
use enum_map::{enum_map, Enum, EnumMap};
use lazy_static::lazy_static;
use PieceColor::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Enum, Hash)]
pub enum PieceColor {
    Black,
    White,
}

lazy_static! {
    /// Colour dependent directional pawn moves for pawns that have already taken their first move
    static ref PAWN_SINGLE_STEPS: EnumMap<PieceColor, Vec<BoardPos>> = enum_map! {
        Black => vec![(-1, 0).into()],
        White => vec![(1, 0).into()]
    };
    /// Colour dependent directional pawn moves for pawns that have not moved yet
    static ref PAWN_DOUBLE_STEPS: EnumMap<PieceColor, Vec<BoardPos>> = enum_map! {
        Black => vec![(-1, 0).into(), (-2, 0).into()],
        White => vec![(1, 0).into(), (2, 0).into()]
    };
    /// Colour dependent directional pawn moves for pawns that can take opponent pieces
    static ref PAWN_TAKES_STEPS: EnumMap<PieceColor, Vec<BoardPos>> = enum_map! {
        Black => vec![(-1, 1).into(), (-1, -1).into()],
        White => vec![(1, 1).into(), (1, -1).into()]
    };
    /// Colour dependent row number to identify promotion squares
    static ref PAWN_PROMOTION_MAP: EnumMap<PieceColor, i8> = enum_map! {
        Black => 0,
        White => 7
    };
    /// Colour dependent row number where the starting board has the pieces
    static ref PIECE_ROWS: EnumMap<PieceColor, i8> = enum_map! {
        Black => 7,
        White => 0
    };
}

impl PieceColor {
    /// Quick query for regular pawn move direction per colour
    #[inline]
    pub fn pawn_single_step(self) -> &'static Vec<BoardPos> {
        &PAWN_SINGLE_STEPS[self]
    }
    /// Quick query for the first pawn move direction per colour
    #[inline]
    pub fn pawn_double_step(self) -> &'static Vec<BoardPos> {
        &PAWN_DOUBLE_STEPS[self]
    }
    /// Quick query for the taking pawn moves per colour
    #[inline]
    pub fn pawn_takes_step(self) -> &'static Vec<BoardPos> {
        &PAWN_TAKES_STEPS[self]
    }
    /// Determine pawn promotion row for a colour
    #[inline]
    pub fn pawn_promotion_row(self) -> i8 {
        PAWN_PROMOTION_MAP[self]
    }
    /// Determine piece starting row for a colour
    #[inline]
    pub fn piece_row(self) -> i8 {
        PIECE_ROWS[self]
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
}
