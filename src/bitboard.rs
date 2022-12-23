/*
 *  ========================================================================
 *  DBCE chess bot, core engine
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

extern crate rand;

use crate::bitboard::PieceKind::*;
use crate::PieceColor::*;
use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::cmp::{max, min};
use std::fmt::{Debug, Display, Formatter};
use std::sync::{Arc, Mutex};

use self::rand::{thread_rng, Rng};

static MATE: f32 = 1000.0;

pub static mut PSBCOUNT: u32 = 0;

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
pub struct PossibleMove {
    pub from: (i8, i8),
    pub to: (i8, i8),
    pub pawnpromotion: Option<PieceKind>,
    pub rook: Option<((i8, i8), (i8, i8))>,
}

impl PossibleMove {
    /*
    Produces moves in uci notation
    */
    fn string(&self) -> String {
        let mut ret = String::with_capacity(5);
        ret.push((self.from.1 as u8 + b'a') as char);
        ret.push((self.from.0 as u8 + b'1') as char);
        ret.push((self.to.1 as u8 + b'a') as char);
        ret.push((self.to.0 as u8 + b'1') as char);
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

/*
The internal representation of the chessboard after a given move.
*/
#[derive(Copy, Clone)]
pub struct PSBoard {
    pub board: [[Option<PieceState>; 8]; 8],
    pub who_moves: PieceColor,
    pub castling: u8,
    pub ep: Option<(i8, i8)>,
    pub move_count: u16,
    pub half_moves_since_pawn: u16,
}

/*
This allows a simple text display of the board on your console. Good for debugging purposes
 */
impl Display for PSBoard {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut retstr = String::new();
        retstr
            .push_str(format!("{:?} to move, move {}\n", self.who_moves, self.move_count).as_str());
        for row in (0..8usize).rev() {
            for col in 0..8usize {
                retstr.push(if let Some(ps) = &self.board[row][col] {
                    format!("{}", ps).pop().unwrap()
                } else {
                    '-'
                });
            }
            retstr.push('\n');
        }
        write!(f, "{}", retstr)
    }
}

impl PSBoard {
    // Creates the standard starting position
    pub fn new() -> PSBoard {
        let mut raw = [[None; 8]; 8];
        for (ridx, row) in raw.iter_mut().enumerate() {
            let (c, onlypawn) = match ridx {
                0 => (White, None),
                1 => (White, Some(Pawn)),
                6 => (Black, Some(Pawn)),
                7 => (Black, None),
                _ => continue,
            };
            for (cidx, asquare) in row.iter_mut().enumerate() {
                *asquare = Some(PieceState {
                    kind: if onlypawn.is_some() {
                        Pawn
                    } else {
                        match cidx {
                            0 | 7 => Rook,
                            1 | 6 => Knight,
                            2 | 5 => Bishop,
                            3 => Queen,
                            4 => King,
                            _ => panic!("Impossible"),
                        }
                    },
                    color: c,
                });
            }
        }
        PSBoard {
            board: raw,
            who_moves: White,
            castling: 15,
            ep: None,
            move_count: 0,
            half_moves_since_pawn: 0,
        }
    }

    pub fn switch_sides(&self) -> PSBoard {
        let mut other_side = *self;
        other_side.who_moves = if self.who_moves == White {
            Black
        } else {
            White
        };
        other_side
    }

    // Allows moves to be translated from lichess to our internal representation
    pub fn make_an_uci_move(&self, themove: &str) -> PSBoard {
        let len = themove.len();
        assert!(len < 6 && len > 3);
        let rawmove = themove.as_bytes();
        let from = ((rawmove[1] - b'1') as i8, (rawmove[0] - b'a') as i8);
        let to = ((rawmove[3] - b'1') as i8, (rawmove[2] - b'a') as i8);
        if len == 5 {
            let promotekind = PieceKind::from_char(rawmove[4] as char);
            self.make_a_move(&PossibleMove {
                from,
                to,
                pawnpromotion: Some(promotekind),
                rook: None,
            })
        } else {
            let movingpiece = self.get_loc(&from).as_ref().unwrap();
            let rookfrom = (from.0, if to.1 == 6 { 7 } else { 0 });
            let rookto = (from.0, if to.1 == 6 { 5 } else { 3 });
            self.make_a_move(&PossibleMove {
                from,
                to,
                pawnpromotion: None,
                rook: if self.castling != 0
                    && (to.0 == 0 || to.0 == 7)
                    && from.1 == 4
                    && (to.1 == 6 || to.1 == 2)
                    && movingpiece.kind == King
                {
                    Some((rookfrom, rookto))
                } else {
                    None
                },
            })
        }
    }

    // Makes a move as per the internal representation
    // Note that the move is not really checked for validity
    // We will produce a completely new internal board representation as the result of the move
    // This will allow further evaluation
    pub fn make_a_move(&self, themove: &PossibleMove) -> PSBoard {
        let mut raw = self.board;
        let prevpiece = self.get_loc(&themove.to);
        {
            let currpiece = raw[themove.from.0 as usize][themove.from.1 as usize]
                .as_ref()
                .unwrap();
            if let Some(ep) = &self.ep {
                if currpiece.kind == Pawn && ep.0 == themove.to.0 && ep.1 == themove.to.1 {
                    // En passant was done, the long move pawn was taken
                    raw[themove.from.0 as usize][themove.to.1 as usize] = None;
                }
            }
        }
        // The move for almost all the cases
        raw[themove.to.0 as usize][themove.to.1 as usize] =
            raw[themove.from.0 as usize][themove.from.1 as usize];
        if let Some(promotion) = &themove.pawnpromotion {
            //When we need to convert a pawn to something
            raw[themove.to.0 as usize][themove.to.1 as usize]
                .as_mut()
                .unwrap()
                .kind = *promotion;
        } else if let Some((from, to)) = &themove.rook {
            // when we are castling, the rook move is also stored
            raw[to.0 as usize][to.1 as usize] = raw[from.0 as usize][from.1 as usize];
            raw[from.0 as usize][from.1 as usize] = None;
        }
        raw[themove.from.0 as usize][themove.from.1 as usize] = None; // Where we left from is now empty
        let currpiece = raw[themove.to.0 as usize][themove.to.1 as usize]
            .as_ref()
            .unwrap();
        PSBoard {
            board: raw,
            who_moves: match currpiece.color {
                White => Black,
                Black => White,
            },
            ep: if currpiece.kind == Pawn
                && ((themove.from.0 as i8) - (themove.to.0 as i8)).abs() == 2
            {
                Some(((themove.from.0 + themove.to.0) >> 1, themove.to.1))
            } else {
                None
            },
            castling: self.castling & {
                if currpiece.kind == King {
                    if currpiece.color == White {
                        3
                    } else {
                        12
                    }
                } else if currpiece.kind == Rook {
                    if themove.from.1 == 0 {
                        if currpiece.color == White {
                            7
                        } else {
                            13
                        }
                    } else if themove.from.1 == 7 {
                        if currpiece.color == White {
                            11
                        } else {
                            14
                        }
                    } else {
                        15
                    }
                } else if (themove.to.0 == 0 || themove.to.0 == 7)
                    && (themove.to.1 == 0 || themove.to.1 == 7)
                {
                    if let Some(taken) = prevpiece {
                        if taken.kind == Rook {
                            let mut rookside = if themove.to.1 == 7 { 1u8 } else { 2 };
                            rookside <<= if themove.to.0 == 0 { 2 } else { 0 };
                            (!rookside) & 15
                        } else {
                            15
                        }
                    } else {
                        15
                    }
                } else {
                    15
                }
            },
            half_moves_since_pawn: self.half_moves_since_pawn + 1,
            move_count: self.move_count + if currpiece.color == Black { 1 } else { 0 },
        }
    }

    // Determines what piece is at a particular location of the board
    #[inline]
    pub fn get_loc(&self, (row, col): &(i8, i8)) -> &Option<PieceState> {
        &self.board[*row as usize][*col as usize]
    }

    // Pieces can move and take till they hit another piece, assuming it is not the same colour
    #[inline]
    fn piece_move_rule(&self, pos: &(i8, i8)) -> bool {
        let target = self.get_loc(pos);
        if let Some(other_piece) = target.as_ref() {
            other_piece.color != self.who_moves
        } else {
            true
        }
    }

    // Pawns can only move to empty spaces, cannot take in forward movement
    #[inline]
    fn pawn_move_rule(&self, pos: &(i8, i8)) -> bool {
        self.get_loc(pos).is_none()
    }

    // Pawns can take in diagonals, even with en passant
    #[inline]
    fn pawn_take_rule(&self, pos: &(i8, i8)) -> bool {
        let target = self.get_loc(pos);
        if let Some(other_piece) = target.as_ref() {
            // regular move
            other_piece.color != self.who_moves
        } else {
            // En passant
            if let Some(ep_loc) = self.ep.as_ref() {
                pos.0 == ep_loc.0 && pos.1 == ep_loc.1
            } else {
                // Nothing to capture
                false
            }
        }
    }

    // Determines if a particular move is allowed based on the piece movement rule and the coordinate validity
    // if so it produces a new move
    #[inline]
    fn fil_map_core<I>(
        &self,
        row: i8,
        col: i8,
        loc: (i8, i8),
        on_board_rule: &I,
    ) -> Option<PossibleMove>
    where
        I: Fn(&Self, &(i8, i8)) -> bool,
    {
        if loc.0 & 7i8 == loc.0 && loc.1 & 7i8 == loc.1 &&
            // Move is on the board
            on_board_rule(self, &loc)
        {
            // Move is allowed by the rule, we generate it
            Some(PossibleMove {
                from: (row, col),
                to: (loc.0, loc.1),
                pawnpromotion: None,
                rook: None,
            })
        } else {
            None
        }
    }

    // Generates a set of valid moves based on the cordinate adjustments passed in via the iterator
    // Immediately deposits the moves in the output vector
    #[inline]
    fn gen_moves_from_dirs<'a, I, J>(
        &self,
        row: i8,
        col: i8,
        on_board_rule: &J,
        possiblemoves: I,
        out: &mut Vec<PossibleMove>,
    ) where
        I: Iterator<Item = &'a (i8, i8)>,
        J: Fn(&Self, &(i8, i8)) -> bool,
    {
        out.extend(
            possiblemoves
                .map(|m| (m.0 + row, m.1 + col))
                .filter_map(|loc| self.fil_map_core(row, col, loc, on_board_rule)),
        );
    }

    // This does the same as gen_moves_from_dirs, but stops at the first occasion
    // when moves are no longer possible
    #[inline]
    fn gen_moves_from_dirs_with_stop<'a, I, J>(
        &self,
        row: i8,
        col: i8,
        on_board_rule: &J,
        possiblemoves: I,
        out: &mut Vec<PossibleMove>,
    ) where
        I: Iterator<Item = &'a (i8, i8)>,
        J: Fn(&Self, &(i8, i8)) -> bool,
    {
        out.extend(
            possiblemoves
                .map(|m| (m.0 + row, m.1 + col))
                .map_while(|loc| self.fil_map_core(row, col, loc, on_board_rule)),
        );
    }

    // This generates moves based on directional vectors (useful for rooks, bishops and qeens)
    fn gen_moves_from_vecs<'a, I>(&self, row: i8, col: i8, vecs: I, out: &mut Vec<PossibleMove>)
    where
        I: Iterator<Item = &'a (i8, i8)>,
    {
        for (x, y) in vecs {
            static DIRECTIONAL_MOVES: [[(i8, i8); 7]; 9] = [
                // this array is laid out so it is easy to map into it with the below formula using just the input coords
                [
                    (-1, -1),
                    (-2, -2),
                    (-3, -3),
                    (-4, -4),
                    (-5, -5),
                    (-6, -6),
                    (-7, -7),
                ],
                [
                    (-1, 0),
                    (-2, 0),
                    (-3, 0),
                    (-4, 0),
                    (-5, 0),
                    (-6, 0),
                    (-7, 0),
                ],
                [
                    (-1, 1),
                    (-2, 2),
                    (-3, 3),
                    (-4, 4),
                    (-5, 5),
                    (-6, 6),
                    (-7, 7),
                ],
                [
                    (0, -1),
                    (0, -2),
                    (0, -3),
                    (0, -4),
                    (0, -5),
                    (0, -6),
                    (0, -7),
                ],
                [
                    // filler to make the x-y mapping easier
                    (0, 0),
                    (0, 0),
                    (0, 0),
                    (0, 0),
                    (0, 0),
                    (0, 0),
                    (0, 0),
                ],
                [(0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7)],
                [
                    (1, -1),
                    (2, -2),
                    (3, -3),
                    (4, -4),
                    (5, -5),
                    (6, -6),
                    (7, -7),
                ],
                [(1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0)],
                [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (6, 6), (7, 7)],
            ];
            let a = DIRECTIONAL_MOVES[(x + y + 2 * x + 4) as usize]; // the input coords directly map into the above array
            let allow_next = RefCell::new(true);
            self.gen_moves_from_dirs_with_stop(
                row,
                col,
                &move |s, m| {
                    if *allow_next.borrow() {
                        // Ensures we can take a piece but not go further or we need to stop a step before our own pieces
                        if let Some(trg) = s.get_loc(m) {
                            let mut d = allow_next.borrow_mut();
                            *d = false;
                            !(trg.color == s.who_moves)
                        } else {
                            true
                        }
                    } else {
                        false
                    }
                },
                a.iter(),
                out,
            );
        }
    }

    // This figures out all the possible moves on the particular board
    pub fn gen_potential_moves(&self, castleallowed: bool, themoves: &mut Vec<PossibleMove>) {
        let (pawndirection, piecerow) = match self.who_moves {
            White => (1i8, 0i8),
            Black => (-1i8, 7i8),
        };
        for row in 0..8 {
            for col in 0..8 {
                if let Some(currpiece) = &self.board[row as usize][col as usize] {
                    if currpiece.color != self.who_moves {
                        continue;
                    }
                    match currpiece.kind {
                        King => {
                            static POSSIBLE_KING_MOVES: [(i8, i8); 8] = [
                                (-1, -1),
                                (-1, 0),
                                (-1, 1),
                                (0, -1),
                                (0, 1),
                                (1, -1),
                                (1, 0),
                                (1, 1),
                            ];
                            self.gen_moves_from_dirs(
                                row,
                                col,
                                &PSBoard::piece_move_rule,
                                POSSIBLE_KING_MOVES.iter(),
                                themoves,
                            );
                            if castleallowed {
                                // Castling:
                                if row == piecerow && col == 4 {
                                    // the king is in its original location we need a more in depth check on castling
                                    if (self.who_moves == White && self.castling & 12 != 0)
                                        || (self.who_moves == Black && self.castling & 3 != 0)
                                    {
                                        // there are castling opportunities
                                        static CASTLING_SIDE: [u8; 2] = [10, 5]; //queen, king
                                        static CASTLING_RANGES: [(usize, usize); 2] =
                                            [(1, 3), (5, 6)];
                                        static CASTLING_MOVES: [[i8; 3]; 2] =
                                            [[2, 0, 3], [6, 7, 5]];
                                        'outer: for (idx, side) in CASTLING_SIDE.iter().enumerate()
                                        {
                                            if self.castling & side != 0 {
                                                let mut castling_range_free = true;
                                                for lc in
                                                    CASTLING_RANGES[idx].0..CASTLING_RANGES[idx].1
                                                {
                                                    castling_range_free &=
                                                        self.board[row as usize][lc].is_none();
                                                    if !castling_range_free {
                                                        continue 'outer;
                                                    }
                                                }

                                                if castling_range_free {
                                                    // Let's see if we would cross a check
                                                    let otherside = self.switch_sides();
                                                    let mincol = min(CASTLING_MOVES[idx][0], 4);
                                                    let maxcol = max(CASTLING_MOVES[idx][0], 4);
                                                    let count = {
                                                        let mut other_side_moves = Vec::new();
                                                        {
                                                            otherside.gen_potential_moves(
                                                                false,
                                                                &mut other_side_moves,
                                                            );
                                                        }
                                                        other_side_moves
                                                            .iter()
                                                            .filter(|m| {
                                                                m.to.0 == piecerow
                                                                    && m.to.1 <= maxcol
                                                                    && m.to.1 >= mincol
                                                            })
                                                            .count()
                                                    };
                                                    if count == 0 {
                                                        // would not cross check
                                                        themoves.push(PossibleMove {
                                                            from: (row, col),
                                                            to: (row, CASTLING_MOVES[idx][0]),
                                                            pawnpromotion: None,
                                                            rook: Some((
                                                                (row, CASTLING_MOVES[idx][1]),
                                                                (row, CASTLING_MOVES[idx][2]),
                                                            )),
                                                        });
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        Pawn => {
                            // normal pawn move
                            let prelen = themoves.len();
                            if row == 1 || row == 6 {
                                // two step pawn move at the beginning
                                self.gen_moves_from_dirs_with_stop(
                                    row,
                                    col,
                                    &PSBoard::pawn_move_rule,
                                    [(pawndirection, 0), (pawndirection * 2, 0)].iter(),
                                    themoves,
                                );
                            } else {
                                self.gen_moves_from_dirs(
                                    row,
                                    col,
                                    &PSBoard::pawn_move_rule,
                                    [(pawndirection, 0)].iter(),
                                    themoves,
                                );
                            }

                            // Pawn takes
                            self.gen_moves_from_dirs(
                                row,
                                col,
                                &PSBoard::pawn_take_rule,
                                [(pawndirection, 1), (pawndirection, -1)].iter(),
                                themoves,
                            );
                            if themoves.len() > prelen {
                                // Promotions handling
                                if themoves[prelen].to.0 == 0 || themoves[prelen].to.0 == 7 {
                                    static PROMOTIONS: [PieceKind; 4] =
                                        [Rook, Knight, Queen, Bishop];
                                    let mut pawnmoves = [None, None, None];
                                    let mut idx = 0;
                                    while themoves.len() != prelen {
                                        pawnmoves[idx] = themoves.pop();
                                        idx += 1;
                                    }
                                    for m in pawnmoves.iter().flatten() {
                                        for pk in PROMOTIONS {
                                            themoves.push(PossibleMove {
                                                from: m.from,
                                                to: m.to,
                                                pawnpromotion: Some(pk),
                                                rook: None,
                                            });
                                        }
                                    }
                                }
                            }
                        }
                        Knight => {
                            static POSSIBLE_KNIGHT_MOVES: [(i8, i8); 8] = [
                                (-1, -2),
                                (-1, 2),
                                (-2, -1),
                                (-2, 1),
                                (1, -2),
                                (1, 2),
                                (2, -1),
                                (2, 1),
                            ];
                            self.gen_moves_from_dirs(
                                row,
                                col,
                                &PSBoard::piece_move_rule,
                                POSSIBLE_KNIGHT_MOVES.iter(),
                                themoves,
                            );
                        }
                        Rook => {
                            static POSSIBLE_ROOK_DIRECTIONS: [(i8, i8); 4] =
                                [(-1, 0), (1, 0), (0, 1), (0, -1)];
                            self.gen_moves_from_vecs(
                                row,
                                col,
                                POSSIBLE_ROOK_DIRECTIONS.iter(),
                                themoves,
                            );
                        }
                        Bishop => {
                            static POSSIBLE_BISHOP_DIRECTIONS: [(i8, i8); 4] =
                                [(-1, -1), (1, 1), (-1, 1), (1, -1)];
                            self.gen_moves_from_vecs(
                                row,
                                col,
                                POSSIBLE_BISHOP_DIRECTIONS.iter(),
                                themoves,
                            );
                        }
                        Queen => {
                            static POSSIBLE_QUEEN_DIRECTIONS: [(i8, i8); 8] = [
                                (-1, -1),
                                (1, 1),
                                (-1, 1),
                                (1, -1),
                                (-1, 0),
                                (1, 0),
                                (0, 1),
                                (0, -1),
                            ];
                            self.gen_moves_from_vecs(
                                row,
                                col,
                                POSSIBLE_QUEEN_DIRECTIONS.iter(),
                                themoves,
                            );
                        }
                    }
                }
            }
        }
    }

    // A simple scoring mechanism which just counts up the pieces and pawns based on their usual values
    pub fn score(&self) -> f32 {
        let mut loc_score = 0f32;
        let mut white_king_found = false;
        let mut black_king_found = false;
        for row in 0..8usize {
            for col in 0..8usize {
                if let Some(currpiece) = &self.board[row][col] {
                    let mult = if currpiece.color == White {
                        1f32
                    } else {
                        -1f32
                    };
                    loc_score += mult
                        * match currpiece.kind {
                            Pawn => 1f32,
                            Knight => 3f32,
                            Bishop => 3.1f32,
                            Rook => 5f32,
                            Queen => 9f32,
                            King => {
                                if currpiece.color == White {
                                    white_king_found = true;
                                } else {
                                    black_king_found = true;
                                }
                                0f32
                            }
                        }
                }
            }
        }
        if white_king_found {
            if black_king_found {
                loc_score as f32
            } else {
                MATE
            }
        } else {
            -MATE
        }
    }
}

pub struct Engine {
    vec_cache: Arc<Mutex<Vec<(Vec<PossibleMove>, Vec<(PossibleMove, f32)>)>>>,
}

impl Engine {
    pub fn new() -> Engine {
        Engine {
            vec_cache: Arc::new(Mutex::new(Vec::new())),
        }
    }

    fn get_cached(&mut self) -> (Vec<PossibleMove>, Vec<(PossibleMove, f32)>) {
        self.vec_cache
            .borrow_mut()
            .lock()
            .unwrap()
            .pop()
            .unwrap_or_else(|| (Vec::new(), Vec::new()))
    }

    fn release_cached(
        &mut self,
        mut moves: Vec<PossibleMove>,
        mut scored_moves: Vec<(PossibleMove, f32)>,
    ) {
        moves.clear();
        scored_moves.clear();
        self.vec_cache
            .borrow_mut()
            .lock()
            .unwrap()
            .push((moves, scored_moves))
    }

    // Determines the best move on the depth asked for
    // If the decide flag is passed, we will have the move generated, otherwise we just use this method for scoring
    pub fn best_move_for(
        &mut self,
        start_board: &PSBoard,
        max_depth: u8,
        decide: bool,
    ) -> (Option<PossibleMove>, f32) {
        // let prefix: String = std::iter::repeat(' ')
        //     .take(4 as usize - max_depth as usize)
        //     .collect();
        // println!("{} ==== Current depth: {}, decide {}\n{}", prefix, max_depth, decide, start_board);
        let sc = start_board.score();
        let mut ret = (None, sc);
        if (sc.abs() - MATE).abs() > 0.1 {
            let (mut moves, mut scores) = self.get_cached();
            assert_eq!(0, moves.len());
            start_board.gen_potential_moves(true, &mut moves);
            let movecount = moves.len();
            //            println!("{} Potential moves: {:?}", prefix, moves);
            if movecount == 0 {
                panic!("Could not generate a single move!!!");
            } else {
                while let Some(curr_move) = moves.pop() {
                    //                    println!("{}  Testing move {:?}", prefix, curr_move);
                    let board_with_move = start_board.make_a_move(&curr_move);
                    unsafe {
                        PSBCOUNT += 1;
                    }
                    let curr_score = if max_depth == 0 {
                        board_with_move.score()
                    } else {
                        (self.best_move_for(&board_with_move, max_depth - 1, false).1 * 10.0 + sc)
                            / 11.0
                    };
                    // println!("{}  Acquired score: {}", prefix, curr_score);
                    scores.push((curr_move, curr_score));
                    if max_depth == 0 {
                        if start_board.who_moves == White {
                            if (curr_score - MATE).abs() < 0.1 {
                                // No need to search further we have a mate
                                break;
                            }
                        } else if (curr_score + MATE).abs() < 0.1 {
                            // No need to search further we have a mate
                            break;
                        }
                    }
                }
                moves.clear();
                let sorting = match start_board.who_moves {
                    White => 1.0,
                    Black => -1.0,
                };
                let lastscore = scores.len() - 1;
                scores.sort_unstable_by(|(_, s1), (_, s2)| {
                    (sorting * s1).partial_cmp(&(s2 * sorting)).unwrap() // UNWRAP CAN FAIL HERE!
                });
                if (scores[lastscore].1.abs() - MATE).abs() < 0.1 {
                    // End of game
                    let best = scores.swap_remove(lastscore);
                    ret = (if decide { Some(best.0) } else { None }, best.1);
                } else {
                    let best = &scores[lastscore];
                    let sameasbest = if (best.1 - scores[0].1).abs() < 0.001 {
                        lastscore
                    } else {
                        let closetoup = scores.binary_search_by(|(_, s)| {
                            (sorting * s)
                                .partial_cmp(&((best.1 + 0.001) * sorting))
                                .unwrap()
                        });
                        let closetodown = scores.binary_search_by(|(_, s)| {
                            (sorting * s)
                                .partial_cmp(&((best.1 - 0.001) * sorting))
                                .unwrap()
                        });
                        let upidx = match closetoup {
                            Ok(loc) => loc,
                            Err(loc) => loc,
                        };
                        let downidx = match closetodown {
                            Ok(loc) => loc,
                            Err(loc) => loc,
                        };
                        upidx.max(downidx) - upidx.min(downidx)
                    };

                    let best = if sameasbest == 0 {
                        scores.pop().unwrap()
                    } else {
                        let mut rng = thread_rng();
                        let idx = rng.gen_range(0..sameasbest);
                        scores.swap_remove(lastscore - idx)
                    };
                    ret = (Some(best.0), best.1)
                }
            }
            self.release_cached(moves, scores);
        }
        ret
    }
}
