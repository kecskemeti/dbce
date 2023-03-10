/*
 *  ========================================================================
 *  DBCE chess bot, human communications allowing manual testing etc.
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
use crate::bitboard::PSBoard;
use crate::board_rep::PieceColor::{Black, White};
use crate::board_rep::{BaseMove, PieceKind, PieceState, PossibleMove};
use ahash::AHashMap;
use std::fmt::{Display, Formatter};

/*
This allows a simple text display of the board on your console. Good for debugging purposes
 */
impl Display for PSBoard {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut return_string = String::new();
        return_string
            .push_str(format!("{:?} to move, move {}\n", self.who_moves, self.move_count).as_str());
        for row in (0..8).rev() {
            for col in 0..8 {
                return_string.push(if let Some(ps) = &self.get_loc((row, col)) {
                    format!("{}", ps).pop().unwrap()
                } else {
                    '-'
                });
            }
            return_string.push('\n');
        }
        write!(f, "{}", return_string)
    }
}

impl PSBoard {
    #[allow(dead_code)]
    // Allows initialising a particular position from fen
    pub fn from_fen(fen: String) -> PSBoard {
        let mut raw = [[None; 8]; 8];
        let mut next_move = None;
        let mut castling = 255u8;
        let mut ep = None;
        let mut half: Option<u16> = None;
        let mut full: Option<u16> = None;
        for (idx, fen_part) in fen.split_whitespace().enumerate() {
            match idx {
                0 => {
                    let mut row = 7usize;
                    let mut col = 0usize;
                    for piece_info in fen_part.chars() {
                        if piece_info.is_ascii_digit() {
                            col += (piece_info as u8 - b'0') as usize;
                        } else if piece_info == '/' {
                            col = 0;
                            if row == 0 {
                                panic!("Should not have more rows on the chessboard!");
                            }
                            row -= 1;
                        } else {
                            let color = if piece_info.is_uppercase() {
                                White
                            } else {
                                Black
                            };
                            let kind = PieceKind::from_char(piece_info);
                            raw[row][col] = Some(PieceState { kind, color });
                            col += 1;
                        }
                    }
                }
                1 => {
                    next_move = Some(match fen_part.chars().next().unwrap() {
                        'w' => White,
                        'b' => Black,
                        _ => panic!("Unexpected color for the next move!"),
                    })
                }
                2 => {
                    castling = 0;
                    for castle_right in fen_part.chars() {
                        match castle_right {
                            'Q' => castling |= 8,
                            'K' => castling |= 4,
                            'q' => castling |= 2,
                            'k' => castling |= 1,
                            '-' => {}
                            _ => panic!("Incorrect castling details"),
                        }
                    }
                }
                3 => {
                    let mut fen_chars = fen_part.chars();
                    let first_char = fen_chars.next().unwrap();
                    if first_char != '-' {
                        ep = Some((
                            first_char as i8 - 'a' as i8,
                            fen_chars.next().unwrap() as i8 - '1' as i8,
                        ));
                    }
                }
                4 => half = Some(fen_part.parse().unwrap()),

                5 => full = Some(fen_part.parse().unwrap()),

                _ => panic!("Too many fields in the FEN"),
            }
        }
        PSBoard {
            board: raw,
            who_moves: if let Some(mover) = next_move {
                mover
            } else {
                panic!("Unspecified whose turn it is!")
            },
            castling: if castling != 255 {
                castling
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
            score: 0f32,
            continuation: AHashMap::new(),
        }
    }

    #[allow(dead_code)]
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
        ret.push(if self.who_moves == White { 'w' } else { 'b' });
        ret.push(' ');
        if self.castling == 0 {
            ret.push('-');
        } else {
            static CASTLING_ORDER: &str = "QKqk";
            static CASTLING_MASKS: [u8; 4] = [8, 4, 2, 1];
            ret.push_str(
                CASTLING_ORDER
                    .chars()
                    .zip(CASTLING_MASKS.iter())
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
    #[allow(dead_code)]
    pub fn make_a_human_move(&mut self, the_move: String) -> Option<PSBoard> {
        let mut rev_move: String = the_move.chars().rev().collect();
        let first_char = rev_move.pop().unwrap();
        let mut col = Vec::new();
        let mut piece_kind = PieceKind::Pawn;
        let mut castling = false;
        let mut internal_move = None;
        match first_char {
            'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' => {
                col.push((first_char as u8 - b'a') as i8)
            }
            'K' | 'Q' | 'N' | 'R' | 'B' => piece_kind = PieceKind::from_char(first_char),
            'O' => castling = true,
            _ => panic!("Unexpected chess notation"),
        }
        if castling {
            let castle_type = the_move.split('-').count();
            let castle_row = if self.who_moves == White { 0 } else { 7 };
            if castle_type == 2 {
                //short
                internal_move = Some(PossibleMove {
                    the_move: BaseMove {
                        from: (castle_row, 4),
                        to: (castle_row, 6),
                    },
                    pawn_promotion: None,
                    rook: Some(BaseMove {
                        from: (castle_row, 7),
                        to: (castle_row, 5),
                    }),
                });
            } else {
                //long
                internal_move = Some(PossibleMove {
                    the_move: BaseMove {
                        from: (castle_row, 4),
                        to: (castle_row, 2),
                    },
                    pawn_promotion: None,
                    rook: Some(BaseMove {
                        from: (castle_row, 0),
                        to: (castle_row, 3),
                    }),
                });
            }
        } else {
            let mut row = Vec::new();
            let mut promotion = false;
            let mut promote_kind = None;
            loop {
                let first_char = rev_move.pop();
                if let Some(consecutive) = first_char {
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
            let mut all_moves = Vec::new();
            self.gen_potential_moves(false, &mut all_moves);
            if row.len() == 1 {
                if col.len() == 1 {
                    let target = (row.pop().unwrap(), col.pop().unwrap());
                    // regular move
                    for a_move in all_moves {
                        let moving_piece = self.get_loc(a_move.the_move.from).as_ref().unwrap();
                        if moving_piece.kind == piece_kind
                            && a_move.the_move.to.0 == target.0
                            && a_move.the_move.to.1 == target.1
                        {
                            if promotion {
                                if let Some(knd) = promote_kind {
                                    if let Some(ppm) = &a_move.pawn_promotion {
                                        if knd == *ppm {
                                            internal_move = Some(a_move);
                                            break;
                                        }
                                    }
                                } else {
                                    internal_move = Some(a_move);
                                    break;
                                }
                            } else {
                                internal_move = Some(a_move);
                                break;
                            }
                        }
                    }
                } else {
                    let target = (row.pop().unwrap(), col.pop().unwrap());
                    let source_col = col.pop().unwrap();
                    for a_move in all_moves {
                        let moving_piece = self.get_loc(a_move.the_move.from).as_ref().unwrap();
                        if moving_piece.kind == piece_kind
                            && a_move.the_move.from.1 == source_col
                            && a_move.the_move.to.0 == target.0
                            && a_move.the_move.to.1 == target.1
                        {
                            if promotion {
                                if let Some(knd) = promote_kind {
                                    if let Some(ppm) = &a_move.pawn_promotion {
                                        if knd == *ppm {
                                            internal_move = Some(a_move);
                                            break;
                                        }
                                    }
                                } else {
                                    internal_move = Some(a_move);
                                    break;
                                }
                            } else {
                                internal_move = Some(a_move);
                                break;
                            }
                        }
                    }
                }
            } else if col.len() == 1 {
                let target = (row.pop().unwrap(), col.pop().unwrap());
                let source_row = row.pop().unwrap();
                for a_move in all_moves {
                    let moving_piece = self.get_loc(a_move.the_move.from).as_ref().unwrap();
                    if moving_piece.kind == piece_kind
                        && a_move.the_move.from.0 == source_row
                        && a_move.the_move.to.0 == target.0
                        && a_move.the_move.to.1 == target.1
                    {
                        internal_move = Some(a_move);
                        break;
                    }
                }
            } else {
                let target = (row.pop().unwrap(), col.pop().unwrap());
                let source = (row.pop().unwrap(), col.pop().unwrap());
                for a_move in all_moves {
                    let moving_piece = self.get_loc(a_move.the_move.from).as_ref().unwrap();
                    if moving_piece.kind == piece_kind
                        && a_move.the_move.from.0 == source.0
                        && a_move.the_move.from.1 == source.1
                        && a_move.the_move.to.0 == target.0
                        && a_move.the_move.to.1 == target.1
                    {
                        internal_move = Some(a_move);
                        break;
                    }
                }
            }
        }
        internal_move.map(|im| self.make_a_move(&im))
    }
}
