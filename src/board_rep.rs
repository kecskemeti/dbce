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
 *  (C) Copyright 2022, Gabor Kecskemeti
 */

use crate::board_rep::PieceColor::*;
use crate::board_rep::PieceKind::*;
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum PieceColor {
    Black,
    White,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum PieceKind {
    King,
    Queen,
    Rook,
    Bishop,
    Knight,
    Pawn,
}

impl PieceKind {
    pub fn from_char(piece: char) -> PieceKind {
        let piecekind = piece.to_ascii_lowercase();
        match piecekind {
            'k' => King,
            'q' => Queen,
            'b' => Bishop,
            'r' => Rook,
            'n' => Knight,
            'p' => Pawn,
            _ => panic!("Unexpected chess piece type"),
        }
    }
    pub fn as_char(&self) -> char {
        match self {
            King => 'k',
            Queen => 'q',
            Bishop => 'b',
            Knight => 'n',
            Rook => 'r',
            Pawn => 'p',
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct PieceState {
    pub kind: PieceKind,
    pub color: PieceColor,
}

impl Display for PieceState {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let uppercase_if_needed: fn(char) -> char = if self.color == White {
            |m| (m as u8 - (b'a' - b'A')) as char
        } else {
            |m| m
        };
        let res = uppercase_if_needed(self.kind.as_char());
        write!(f, "{}", res)
    }
}

#[derive(Clone)]
pub struct BaseMove {
    pub from: (i8, i8),
    pub to: (i8, i8),
}

#[derive(Clone)]
pub struct PossibleMove {
    pub themove: BaseMove,
    pub pawnpromotion: Option<PieceKind>,
    pub rook: Option<BaseMove>,
}

impl PossibleMove {
    /*
    Produces moves in uci notation
    */
    fn string(&self) -> String {
        let mut ret = String::with_capacity(5);
        ret.push((self.themove.from.1 as u8 + b'a') as char);
        ret.push((self.themove.from.0 as u8 + b'1') as char);
        ret.push((self.themove.to.1 as u8 + b'a') as char);
        ret.push((self.themove.to.0 as u8 + b'1') as char);
        if let Some(pp) = &self.pawnpromotion {
            ret.push(pp.as_char());
        }
        ret
    }
}

impl Display for PossibleMove {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string())
    }
}

impl Debug for PossibleMove {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string())
    }
}
