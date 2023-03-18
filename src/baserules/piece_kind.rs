/*
 *  ========================================================================
 *  DBCE chess bot, simple chess piece representation structures
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
use PieceKind::*;

/// All chess piece types
#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq, Enum)]
pub enum PieceKind {
    King,
    Queen,
    Rook,
    Bishop,
    Knight,
    Pawn,
}

/// This array is laid out sparsely to accommodate each ascii letter representing the characters from 'b' to 'r'
static CHAR_PIECE_MAP: [Option<PieceKind>; 17] = [
    Some(Bishop),
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    Some(King),
    None,
    None,
    Some(Knight),
    None,
    Some(Pawn),
    Some(Queen),
    Some(Rook),
];

lazy_static! {
    /// Lists all possible moves for the pieces or all possible directions if pieces can slide across the board
    /// For kings and knights it is all possible relative moves compared to their current square
    /// For bishops, rooks and queens it is listing directional vectors that point towards the pieces possible future positions achievable in a single step
    static ref PIECE_MOVES: EnumMap<PieceKind, Vec<BoardPos>> = enum_map! {
        Bishop => vec![(-1, -1), (1, 1), (-1, 1), (1, -1)],
        Rook => vec![(-1, 0), (1, 0), (0, 1), (0, -1)],
        Knight => vec![
            (-1, -2),
            (-1, 2),
            (-2, -1),
            (-2, 1),
            (1, -2),
            (1, 2),
            (2, -1),
            (2, 1),
        ],
        King | Queen => vec![
        (-1, -1),
        (1, 1),
        (-1, 1),
        (1, -1),
        (-1, 0),
        (1, 0),
        (0, 1),
        (0, -1),
        ],
        Pawn => Vec::new(),
    };
    static ref PIECE_CHARS: EnumMap<PieceKind, char> = enum_map! {
            King => 'k',
            Queen => 'q',
            Bishop => 'b',
            Knight => 'n',
            Rook => 'r',
            Pawn => 'p',
    };
}

impl PieceKind {
    /// Allows pieces to be determined based on uci and classical chess notation
    /// # Panics
    /// When we receive a character to translate that we cannot translate to the usual notation
    pub fn from_char(piece: char) -> PieceKind {
        let idx: usize = (piece.to_ascii_lowercase() as i8 - b'b' as i8)
            .try_into()
            .unwrap_or_else(|_| panic!("Unmappable character received: {piece}"));
        CHAR_PIECE_MAP[idx].expect("Unexpected chess piece type")
    }
    /// Converts a piece type back to a character form usable for chess notation
    pub fn to_char(self) -> char {
        PIECE_CHARS[self]
    }
    /// Allows querying the kinds of moves we can make with a particular piece. Note pawn moves are handled differently as they depend on the colour and state of the pawn
    pub fn vec_moves(self) -> &'static Vec<BoardPos> {
        &PIECE_MOVES[self]
    }
}
