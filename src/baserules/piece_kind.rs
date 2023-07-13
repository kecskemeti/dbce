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
use crate::baserules::board::PSBoard;
use crate::baserules::board_rep::PossibleMove;
use crate::baserules::positions::{AbsoluteBoardPos, RelativeBoardPos};
use enum_iterator::{all, Sequence};
use enum_map::{enum_map, Enum, EnumMap};
use lazy_static::lazy_static;
use PieceKind::*;

/// All chess piece types
#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq, Enum, Sequence)]
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
    Some(Bishop), //b
    None,         //c
    None,         //d
    None,         //e
    None,         //f
    None,         //g
    None,         //h
    None,         //i
    None,         //j
    Some(King),   //k
    None,         //l
    None,         //m
    Some(Knight), //n
    None,         //o
    Some(Pawn),   //p
    Some(Queen),  //q
    Some(Rook),   //r
];

lazy_static! {
    /// Lists all possible moves for the pieces or all possible directions if pieces can slide across the board
    /// For kings and knights it is all possible relative moves compared to their current square
    /// For bishops, rooks and queens it is listing directional vectors that point towards the pieces possible future positions achievable in a single step
    static ref PIECE_MOVES: EnumMap<PieceKind, Vec<RelativeBoardPos>> = enum_map! {
        Bishop => RelativeBoardPos::transform_to_vec([(-1, -1), (1, 1), (-1, 1), (1, -1)]),
        Rook => RelativeBoardPos::transform_to_vec([(-1, 0), (1, 0), (0, 1), (0, -1)]),
        Knight => RelativeBoardPos::transform_to_vec([(-1, -2), (-1, 2), (-2, -1), (-2, 1), (1, -2), (1, 2), (2, -1), (2, 1),]),
        King | Queen => RelativeBoardPos::transform_to_vec([(-1, -1), (1, 1), (-1, 1), (1, -1), (-1, 0), (1, 0), (0, 1), (0, -1),]),
        Pawn => Vec::new(),
    };
    static ref U8_PIECE_MAP: [Option<PieceKind>; 7] = {
        let mut ret = [None; 7];
        all::<PieceKind>().for_each(|k| ret[k.to_u8() as usize]=Some(k));
        ret
    };

}

impl PieceKind {
    /// Allows pieces to be determined based on uci and classical chess notation
    /// # Panics
    /// When we receive a character to translate that we cannot translate to the usual notation
    ///
    /// # Example
    /// ```
    /// use dbce::baserules::piece_kind::PieceKind;
    /// let bishop_as_char = 'b';
    /// let bishop = PieceKind::from_char(bishop_as_char);
    /// assert_eq!(bishop_as_char, bishop.to_char());
    /// ```
    pub fn from_char(piece: char) -> PieceKind {
        let idx: usize = (piece.to_ascii_lowercase() as i8 - b'b' as i8)
            .try_into()
            .unwrap_or_else(|_| panic!("Unmappable character received: {piece}"));
        CHAR_PIECE_MAP[idx].expect("Unexpected chess piece type")
    }

    /// Converts a piece type back to a character form usable for chess notation
    #[inline]
    pub fn to_char(self) -> char {
        match self {
            King => 'k',
            Queen => 'q',
            Bishop => 'b',
            Knight => 'n',
            Rook => 'r',
            Pawn => 'p',
        }
    }

    /// Allows querying the kinds of moves we can make with a particular piece. Note pawn moves are handled differently as they depend on the colour and state of the pawn
    #[inline]
    pub fn vec_moves(self) -> &'static Vec<RelativeBoardPos> {
        &PIECE_MOVES[self]
    }

    #[inline]
    pub fn from_u8(piece: u8) -> Option<PieceKind> {
        U8_PIECE_MAP[(piece & 0b111) as usize]
    }

    #[inline]
    pub fn to_u8(self) -> u8 {
        match self {
            King => 1,
            Queen => 2,
            Bishop => 3,
            Knight => 4,
            Rook => 5,
            Pawn => 6,
        }
    }

    #[inline]
    pub fn gen_moves(
        &self,
        board: &PSBoard,
        pos: AbsoluteBoardPos,
        the_moves: &mut Vec<PossibleMove>,
    ) {
        match self {
            Pawn => board.gen_pawn_moves(pos, the_moves),
            King => board.gen_king_moves(pos, the_moves),
            // The knight moves are not directional, so we handle them specifically
            Knight => board.gen_moves_from_dirs(
                pos,
                &PSBoard::piece_move_rule,
                self.vec_moves(),
                the_moves,
            ),
            // All other pieces have simple directional movement, so we just use their directions to generate their possible moves
            Bishop | Rook | Queen => board.gen_moves_from_vecs(pos, self.vec_moves(), the_moves),
        }
    }
}
