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

use std::cell::RefCell;
use std::cmp::{max, min};
use std::fmt::{Debug, Display, Formatter};

use self::rand::{thread_rng, Rng};

static CASTLINGORDER: &str = "QKqk";
static CASTLINGMASKS: [u8; 4] = [8, 4, 2, 1];
static MATE: f32 = 1000.0;

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
            'k' => PieceKind::King,
            'q' => PieceKind::Queen,
            'b' => PieceKind::Bishop,
            'r' => PieceKind::Rook,
            'n' => PieceKind::Knight,
            'p' => PieceKind::Pawn,
            _ => panic!("Unexpected chess piece type"),
        }
    }
    pub fn to_char(&self) -> char {
        match self {
            PieceKind::King => 'k',
            PieceKind::Queen => 'q',
            PieceKind::Bishop => 'b',
            PieceKind::Knight => 'n',
            PieceKind::Rook => 'r',
            PieceKind::Pawn => 'p',
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct PieceState {
    kind: PieceKind,
    color: PieceColor,
}

impl Display for PieceState {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let uppercase_if_needed: fn(char) -> char = if self.color == PieceColor::White {
            |m| (m as u8 - (b'a' - b'A')) as char
        } else {
            |m| m
        };
        let res = uppercase_if_needed(self.kind.to_char());
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
            ret.push(pp.to_char());
        }
        ret
    }
}

impl Display for PossibleMove {
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
                0 => (PieceColor::White, None),
                1 => (PieceColor::White, Some(PieceKind::Pawn)),
                6 => (PieceColor::Black, Some(PieceKind::Pawn)),
                7 => (PieceColor::Black, None),
                _ => continue,
            };
            for (cidx, asquare) in row.iter_mut().enumerate() {
                *asquare = Some(PieceState {
                    kind: if onlypawn.is_some() {
                        PieceKind::Pawn
                    } else {
                        match cidx {
                            0 | 7 => PieceKind::Rook,
                            1 | 6 => PieceKind::Knight,
                            2 | 5 => PieceKind::Bishop,
                            3 => PieceKind::Queen,
                            4 => PieceKind::King,
                            _ => panic!("Impossible"),
                        }
                    },
                    color: c,
                });
            }
        }
        PSBoard {
            board: raw,
            who_moves: PieceColor::White,
            castling: 15,
            ep: None,
            move_count: 0,
            half_moves_since_pawn: 0,
        }
    }

    // Allows initialising a particular position from fen
    pub fn from_fen(fen: String) -> PSBoard {
        let mut raw = [[None; 8]; 8];
        let mut nextmove = None;
        let mut castl = 255u8;
        let mut ep = None;
        let mut half: Option<u16> = None;
        let mut full: Option<u16> = None;
        for (idx, fenpart) in fen.split_whitespace().enumerate() {
            match idx {
                0 => {
                    let mut row = 7usize;
                    let mut col = 0usize;
                    for pieceinfo in fenpart.chars() {
                        if pieceinfo.is_digit(10) {
                            col += (pieceinfo as u8 - b'0') as usize;
                        } else if pieceinfo == '/' {
                            col = 0;
                            if row == 0 {
                                panic!("Should not have more rows on the chessboard!");
                            }
                            row -= 1;
                        } else {
                            let color = if pieceinfo.is_uppercase() {
                                PieceColor::White
                            } else {
                                PieceColor::Black
                            };
                            let kind = PieceKind::from_char(pieceinfo);
                            raw[row][col] = Some(PieceState { kind, color });
                            col += 1;
                        }
                    }
                }
                1 => {
                    nextmove = Some(match fenpart.chars().next().unwrap() {
                        'w' => PieceColor::White,
                        'b' => PieceColor::Black,
                        _ => panic!("Unexpected color for the next move!"),
                    })
                }
                2 => {
                    castl = 0;
                    for castle_right in fenpart.chars() {
                        match castle_right {
                            'Q' => castl |= 8,
                            'K' => castl |= 4,
                            'q' => castl |= 2,
                            'k' => castl |= 1,
                            '-' => {}
                            _ => panic!("Incorrect castling details"),
                        }
                    }
                }
                3 => {
                    let mut fenchars = fenpart.chars();
                    let firstchar = fenchars.next().unwrap();
                    if firstchar != '-' {
                        ep = Some((
                            firstchar as i8 - 'a' as i8,
                            fenchars.next().unwrap() as i8 - '1' as i8,
                        ));
                    }
                }
                4 => half = Some(fenpart.parse().unwrap()),

                5 => full = Some(fenpart.parse().unwrap()),

                _ => panic!("Too many fields in the FEN"),
            }
        }
        PSBoard {
            board: raw,
            who_moves: if let Some(mover) = nextmove {
                mover
            } else {
                panic!("Unspecified whose turn it is!")
            },
            castling: if castl != 255 {
                castl
            } else {
                panic!("Castling details never came!")
            },
            ep,
            move_count: if let Some(mc) = full {
                mc
            } else {
                panic!("Unspecified move count")
            },
            half_moves_since_pawn: if let Some(mc) = half {
                mc
            } else {
                panic!("Unspecified half move count")
            },
        }
    }

    // Allows exporting a PSBoard to fen for external analysis
    pub fn to_fen(&self) -> String {
        let mut ret = String::new();
        let mut since_piece: u8;
        for row in self.board.iter().rev() {
            since_piece = 0;
            for col in row.iter() {
                if let Some(ps) = col {
                    if since_piece != 0 {
                        ret.push((since_piece + b'0') as char);
                    }
                    ret.push(format!("{}", ps).pop().unwrap());
                    since_piece = 0;
                } else {
                    since_piece += 1;
                }
            }
            if since_piece != 0 {
                ret.push((since_piece + b'0') as char);
            }
            ret.push('/');
        }
        ret.pop();
        ret.push(' ');
        ret.push(if self.who_moves == PieceColor::White {
            'w'
        } else {
            'b'
        });
        ret.push(' ');
        if self.castling == 0 {
            ret.push('-');
        } else {
            ret.push_str(
                CASTLINGORDER
                    .chars()
                    .zip(CASTLINGMASKS.iter())
                    .filter_map(|(sym, mask)| {
                        if self.castling & mask == *mask {
                            Some(sym)
                        } else {
                            None
                        }
                    })
                    .collect::<String>()
                    .as_str(),
            );
        }
        ret.push(' ');
        if let Some((row, col)) = &self.ep {
            ret.push((*col as u8 + b'a') as char);
            ret.push((*row as u8 + b'1') as char);
        } else {
            ret.push('-');
        }
        ret.push_str(format!(" {} {}", self.half_moves_since_pawn, self.move_count).as_str());
        ret
    }

    // Reads short algebraic notation and translates it tou our internal structures
    pub fn make_a_human_move(&self, themove: String) -> Option<PSBoard> {
        let mut revstr: String = themove.chars().rev().collect();
        let firstchar = revstr.pop().unwrap();
        let mut col = Vec::new();
        let mut piecekind = PieceKind::Pawn;
        let mut castling = false;
        let mut internal_move = None;
        match firstchar {
            'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' => {
                col.push((firstchar as u8 - b'a') as i8)
            }
            'K' | 'Q' | 'N' | 'R' | 'B' => piecekind = PieceKind::from_char(firstchar),
            'O' => castling = true,
            _ => panic!("Unexpected chess notation"),
        }
        if castling {
            let castletype = themove.split('-').count();
            let castlerow = if self.who_moves == PieceColor::White {
                0
            } else {
                7
            };
            if castletype == 2 {
                //short
                internal_move = Some(PossibleMove {
                    from: (castlerow, 4),
                    to: (castlerow, 6),
                    pawnpromotion: None,
                    rook: Some(((castlerow, 7), (castlerow, 5))),
                });
            } else {
                //long
                internal_move = Some(PossibleMove {
                    from: (castlerow, 4),
                    to: (castlerow, 2),
                    pawnpromotion: None,
                    rook: Some(((castlerow, 0), (castlerow, 3))),
                });
            }
        } else {
            let mut row = Vec::new();
            let mut promotion = false;
            let mut promote_kind = None;
            loop {
                let firstchar = revstr.pop();
                if let Some(consecutive) = firstchar {
                    match consecutive {
                        'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' => {
                            col.push((consecutive as u8 - b'a') as i8)
                        }
                        'x' => {}
                        '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' => {
                            row.push((consecutive as u8 - b'1') as i8)
                        }
                        '=' => promotion = true,
                        'Q' | 'R' | 'B' | 'K' => {
                            if promotion {
                                promote_kind = Some(PieceKind::from_char(consecutive));
                            }
                        }
                        _ => panic!("Unexpected notation in second char"),
                    }
                } else {
                    break;
                }
            }
            let allmoves = self.gen_potential_moves(false);
            if row.len() == 1 {
                if col.len() == 1 {
                    let target = (row.pop().unwrap(), col.pop().unwrap());
                    // regular move
                    for amove in allmoves {
                        let movingpiece = self.get_loc(&amove.from).as_ref().unwrap();
                        if movingpiece.kind == piecekind
                            && amove.to.0 == target.0
                            && amove.to.1 == target.1
                        {
                            if promotion {
                                if let Some(knd) = promote_kind {
                                    if let Some(ppm) = &amove.pawnpromotion {
                                        if knd == *ppm {
                                            internal_move = Some(amove);
                                            break;
                                        }
                                    }
                                } else {
                                    internal_move = Some(amove);
                                    break;
                                }
                            } else {
                                internal_move = Some(amove);
                                break;
                            }
                        }
                    }
                } else {
                    let target = (row.pop().unwrap(), col.pop().unwrap());
                    let sourcecol = col.pop().unwrap();
                    for amove in allmoves {
                        let movingpiece = self.get_loc(&amove.from).as_ref().unwrap();
                        if movingpiece.kind == piecekind
                            && amove.from.1 == sourcecol
                            && amove.to.0 == target.0
                            && amove.to.1 == target.1
                        {
                            if promotion {
                                if let Some(knd) = promote_kind {
                                    if let Some(ppm) = &amove.pawnpromotion {
                                        if knd == *ppm {
                                            internal_move = Some(amove);
                                            break;
                                        }
                                    }
                                } else {
                                    internal_move = Some(amove);
                                    break;
                                }
                            } else {
                                internal_move = Some(amove);
                                break;
                            }
                        }
                    }
                }
            } else if col.len() == 1 {
                let target = (row.pop().unwrap(), col.pop().unwrap());
                let sourcerow = row.pop().unwrap();
                for amove in allmoves {
                    let movingpiece = self.get_loc(&amove.from).as_ref().unwrap();
                    if movingpiece.kind == piecekind
                        && amove.from.0 == sourcerow
                        && amove.to.0 == target.0
                        && amove.to.1 == target.1
                    {
                        internal_move = Some(amove);
                        break;
                    }
                }
            } else {
                let target = (row.pop().unwrap(), col.pop().unwrap());
                let source = (row.pop().unwrap(), col.pop().unwrap());
                for amove in allmoves {
                    let movingpiece = self.get_loc(&amove.from).as_ref().unwrap();
                    if movingpiece.kind == piecekind
                        && amove.from.0 == source.0
                        && amove.from.1 == source.1
                        && amove.to.0 == target.0
                        && amove.to.1 == target.1
                    {
                        internal_move = Some(amove);
                        break;
                    }
                }
            }
        }
        internal_move.map(|im| self.make_a_move(&im))
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
                    && movingpiece.kind == PieceKind::King
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
                PieceColor::White => PieceColor::Black,
                PieceColor::Black => PieceColor::White,
            },
            ep: if currpiece.kind == PieceKind::Pawn
                && ((themove.from.0 as i8) - (themove.to.0 as i8)).abs() == 2
            {
                Some(((themove.from.0 + themove.to.0) >> 1, themove.to.1))
            } else {
                None
            },
            castling: self.castling & {
                if currpiece.kind == PieceKind::King {
                    if currpiece.color == PieceColor::White {
                        3u8
                    } else {
                        12u8
                    }
                } else if currpiece.kind == PieceKind::Rook {
                    if themove.from.1 == 0 {
                        if currpiece.color == PieceColor::White {
                            7u8
                        } else {
                            13u8
                        }
                    } else if themove.from.1 == 7 {
                        if currpiece.color == PieceColor::White {
                            11u8
                        } else {
                            14u8
                        }
                    } else {
                        15u8
                    }
                } else {
                    15u8
                }
            },
            half_moves_since_pawn: self.half_moves_since_pawn + 1,
            move_count: self.move_count
                + if currpiece.color == PieceColor::Black {
                    1
                } else {
                    0
                },
        }
    }

    // Determines what piece is at a particular location of the board
    #[inline]
    fn get_loc(&self, (row, col): &(i8, i8)) -> &Option<PieceState> {
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
                pos.0 == ep_loc.0 as i8 && pos.1 == ep_loc.1 as i8
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
    pub fn gen_potential_moves(&self, castleallowed: bool) -> Vec<PossibleMove> {
        let (pawndirection, piecerow) = match self.who_moves {
            PieceColor::White => (1i8, 0i8),
            PieceColor::Black => (-1i8, 7i8),
        };
        let mut themoves = Vec::with_capacity(100);
        for row in 0..8 {
            for col in 0..8 {
                if let Some(currpiece) = &self.board[row as usize][col as usize] {
                    if currpiece.color != self.who_moves {
                        continue;
                    }
                    match currpiece.kind {
                        PieceKind::King => {
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
                                &mut themoves,
                            );
                            if castleallowed {
                                // Castling:
                                if row == piecerow && col == 4 {
                                    // the king is in its original location we need a more in depth check on castling
                                    if (self.who_moves == PieceColor::White
                                        && self.castling & 12 != 0)
                                        || (self.who_moves == PieceColor::Black
                                            && self.castling & 3 != 0)
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
                                                    let mut otherside = *self;
                                                    otherside.who_moves =
                                                        if self.who_moves == PieceColor::White {
                                                            PieceColor::Black
                                                        } else {
                                                            PieceColor::White
                                                        };
                                                    let mincol = min(CASTLING_MOVES[idx][0], 4);
                                                    let maxcol = max(CASTLING_MOVES[idx][0], 4);
                                                    let count = otherside
                                                        .gen_potential_moves(false)
                                                        .iter()
                                                        .filter(|m| {
                                                            m.to.0 == piecerow
                                                                && m.to.1 <= maxcol
                                                                && m.to.1 >= mincol
                                                        })
                                                        .count();
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
                        PieceKind::Pawn => {
                            // normal pawn move
                            let prelen = themoves.len();
                            if row == 1 || row == 6 {
                                // two step pawn move at the beginning
                                self.gen_moves_from_dirs_with_stop(
                                    row,
                                    col,
                                    &PSBoard::pawn_move_rule,
                                    [(pawndirection, 0), (pawndirection * 2, 0)].iter(),
                                    &mut themoves,
                                );
                            } else {
                                self.gen_moves_from_dirs(
                                    row,
                                    col,
                                    &PSBoard::pawn_move_rule,
                                    [(pawndirection, 0)].iter(),
                                    &mut themoves,
                                );
                            }

                            // Pawn takes
                            self.gen_moves_from_dirs(
                                row,
                                col,
                                &PSBoard::pawn_take_rule,
                                [(pawndirection, 1), (pawndirection, -1)].iter(),
                                &mut themoves,
                            );
                            if themoves.len() > prelen {
                                // Promotions handling
                                if themoves[prelen].to.0 == 0 || themoves[prelen].to.0 == 7 {
                                    static PROMOTIONS: [PieceKind; 4] = [
                                        PieceKind::Rook,
                                        PieceKind::Knight,
                                        PieceKind::Queen,
                                        PieceKind::Bishop,
                                    ];
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
                        PieceKind::Knight => {
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
                                &mut themoves,
                            );
                        }
                        PieceKind::Rook => {
                            static POSSIBLE_ROOK_DIRECTIONS: [(i8, i8); 4] =
                                [(-1, 0), (1, 0), (0, 1), (0, -1)];
                            self.gen_moves_from_vecs(
                                row,
                                col,
                                POSSIBLE_ROOK_DIRECTIONS.iter(),
                                &mut themoves,
                            );
                        }
                        PieceKind::Bishop => {
                            static POSSIBLE_BISHOP_DIRECTIONS: [(i8, i8); 4] =
                                [(-1, -1), (1, 1), (-1, 1), (1, -1)];
                            self.gen_moves_from_vecs(
                                row,
                                col,
                                POSSIBLE_BISHOP_DIRECTIONS.iter(),
                                &mut themoves,
                            );
                        }
                        PieceKind::Queen => {
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
                                &mut themoves,
                            );
                        }
                    }
                }
            }
        }
        themoves
    }

    // A simple scoring mechanism which just counts up the pieces and pawns based on their usual values
    pub fn score(&self) -> f32 {
        let mut loc_score = 0;
        let mut white_king_found = false;
        let mut black_king_found = false;
        for row in 0..8usize {
            for col in 0..8usize {
                if let Some(currpiece) = &self.board[row][col] {
                    let mult = if currpiece.color == PieceColor::White {
                        1
                    } else {
                        -1i8
                    };
                    loc_score += mult
                        * match currpiece.kind {
                            PieceKind::Pawn => 1,
                            PieceKind::Knight | PieceKind::Bishop => 3,
                            PieceKind::Rook => 5,
                            PieceKind::Queen => 9,
                            PieceKind::King => {
                                if currpiece.color == PieceColor::White {
                                    white_king_found = true;
                                } else {
                                    black_king_found = true;
                                }
                                0
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

// Determines the best move on the depth asked for
// If the decide flag is passed, we will have the move generated, otherwise we just use this method for scoring
pub fn best_move_for(
    start_board: &PSBoard,
    max_depth: u8,
    decide: bool,
) -> (Option<PossibleMove>, f32) {
    // let prefix: String = std::iter::repeat(' ').take(4 as usize - max_depth as usize).collect();
    // println!("{} ==== Current depth: {}, decide {}\n{}", prefix, max_depth, decide, start_board);
    let sc = start_board.score();
    if (sc.abs() - MATE).abs() > 0.1 {
        let moves = start_board.gen_potential_moves(true);
        let movecount = moves.len();
        // println!("{} Potential moves: {:?}", prefix, moves);
        if movecount == 0 {
            panic!("Could not generate a single move!!!");
        } else {
            let mut scores = Vec::with_capacity(movecount);
            for curr_move in moves {
                // println!("{}  Testing move {:?}", prefix, curr_move);
                let board_with_move = start_board.make_a_move(&curr_move);
                let curr_score = if max_depth == 0 {
                    board_with_move.score()
                } else {
                    (best_move_for(&board_with_move, max_depth - 1, false).1 * 10.0 + sc) / 11.0
                };
                // println!("{}  Acquired score: {}", prefix, curr_score);
                scores.push((curr_move, curr_score));
                if max_depth == 0 {
                    if start_board.who_moves == PieceColor::White {
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
            let sorting = match start_board.who_moves {
                PieceColor::White => 1.0,
                PieceColor::Black => -1.0,
            };
            let lastscore = scores.len() - 1;
            scores.sort_unstable_by(|(_, s1), (_, s2)| {
                (sorting * s1).partial_cmp(&(s2 * sorting)).unwrap()
            });
            if (scores[lastscore].1.abs() - MATE).abs() < 0.1 {
                // End of game
                let best = scores.swap_remove(lastscore);
                (if decide { Some(best.0) } else { None }, best.1)
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
                (Some(best.0), best.1)
            }
        }
    } else {
        // Already decided board
        (None, sc)
    }
}
