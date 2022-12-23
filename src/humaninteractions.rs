/*
 *  ========================================================================
 *  DBCE chess bot, human comms allowing manual testing etc.
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
use crate::bitboard::PieceColor::{Black, White};
use crate::bitboard::{PSBoard, PieceKind, PieceState, PossibleMove};

impl PSBoard {
    #[allow(dead_code)]
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
                                White
                            } else {
                                Black
                            };
                            let kind = PieceKind::from_char(pieceinfo);
                            raw[row][col] = Some(PieceState { kind, color });
                            col += 1;
                        }
                    }
                }
                1 => {
                    nextmove = Some(match fenpart.chars().next().unwrap() {
                        'w' => White,
                        'b' => Black,
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
            static CASTLINGORDER: &str = "QKqk";
            static CASTLINGMASKS: [u8; 4] = [8, 4, 2, 1];
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
    #[allow(dead_code)]
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
            let castlerow = if self.who_moves == White { 0 } else { 7 };
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
            let mut allmoves = Vec::new();
            self.gen_potential_moves(false, &mut allmoves);
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
}
