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
 *  (C) Copyright 2022-3, Gabor Kecskemeti
 */
use crate::baserules::board::Castling::{
    BlackKingSide, BlackQueenSide, WhiteKingSide, WhiteQueenSide,
};
use crate::baserules::board::{Castling, PSBoard};
use crate::baserules::board_rep::BoardPos;
use crate::baserules::piece_color::PieceColor::*;
use crate::baserules::piece_kind::PieceKind;
use crate::baserules::piece_kind::PieceKind::*;
use crate::baserules::piece_state::PieceState;
use crate::baserules::rawboard::RawBoard;
use enumset::EnumSet;
use std::fmt::{Display, Formatter};

impl PieceState {
    pub fn from_unicode(unicode_char: char) -> PieceState {
        PieceState::from_char(match unicode_char {
            '♔' => 'K',
            '♕' => 'Q',
            '♖' => 'R',
            '♗' => 'B',
            '♘' => 'N',
            '♙' => 'P',
            '♚' => 'k',
            '♛' => 'q',
            '♜' => 'r',
            '♝' => 'b',
            '♞' => 'n',
            '♟' => 'p',
            _ => panic!("Unknown piece!"),
        })
    }

    pub fn to_unicode(&self) -> char {
        let mut as_char = format!(
            "{}",
            match self.kind {
                King => '♔',
                Queen => '♕',
                Rook => '♖',
                Bishop => '♗',
                Knight => '♘',
                Pawn => '♙',
            }
        );
        if self.color == Black {
            let mut a = as_char.into_bytes();
            let last = a.len() - 1;
            a[last] += 6;
            as_char = String::from_utf8(a).expect("Cannot transform back");
        }
        as_char.chars().next().unwrap()
    }
}

impl Display for PSBoard {
    /// This allows a simple text display of the board on your console. Good for debugging purposes
    ///
    /// # Example:
    /// ```
    /// use dbce::baserules::board::PSBoard;
    /// let immortal_game = PSBoard::from_fen("r1bk3r/p2pBpNp/n4n2/1p1NP2P/6P1/3P4/P1P1K3/q5b1 b - - 0 1");
    /// let formatted = format!("{immortal_game}");
    /// assert_eq!("Black to move, move 1\n  abcdefgh\n8 ♜-♝♚---♜ 8\n7 ♟--♟♗♟♘♟ 7\n6 ♞----♞-- 6\n5 -♟-♘♙--♙ 5\n4 ------♙- 4\n3 ---♙---- 3\n2 ♙-♙-♔--- 2\n1 ♛-----♝- 1\n  abcdefgh\n",formatted);
    /// ```
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        static COL: &str = "  abcdefgh\n";
        let mut return_string = String::new();
        return_string.push_str(
            format!(
                "{:?} to move, move {}\n{COL}",
                self.who_moves, self.move_count
            )
            .as_str(),
        );
        for row in (0..8).rev() {
            for col in 0..8 {
                let row_as_str = (row + 1).to_string();
                let (prefix, suffix) = if col == 0 {
                    (format!("{} ", row_as_str), String::new())
                } else if col == 7 {
                    (String::new(), format!(" {}", row_as_str))
                } else {
                    (String::new(), String::new())
                };
                return_string.push_str(&format!(
                    "{}{}{}",
                    prefix,
                    if let Some(ps) = &self.get_loc(BoardPos(row, col)) {
                        format!("{}", ps.to_unicode()).pop().unwrap()
                    } else {
                        '-'
                    },
                    suffix
                ));
            }
            return_string.push('\n');
        }
        return_string.push_str(COL);
        write!(f, "{return_string}")
    }
}

impl PSBoard {
    /// Allows initialising a particular position from fen
    /// <https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation>
    /// # Panics
    /// When the input string is an incorrect fen
    pub fn from_fen(fen: &str) -> PSBoard {
        let mut raw = RawBoard::empty();
        let mut next_move = None;
        let mut castling = EnumSet::empty();
        let mut ep = None;
        let mut half: Option<u16> = None;
        let mut full: Option<u16> = None;
        for (idx, fen_part) in fen.split_whitespace().enumerate() {
            match idx {
                0 => {
                    let mut row = 7;
                    let mut col = 0;
                    for piece_info in fen_part.chars() {
                        if piece_info.is_ascii_digit() {
                            col += piece_info as i8 - b'0' as i8;
                        } else if piece_info == '/' {
                            col = 0;
                            assert_ne!(row, 0, "Should not have more rows on the chessboard!");
                            row -= 1;
                        } else {
                            let color = if piece_info.is_uppercase() {
                                White
                            } else {
                                Black
                            };
                            let kind = PieceKind::from_char(piece_info);
                            raw.set_loc(BoardPos(row, col), &Some(PieceState { kind, color }));
                            col += 1;
                        }
                    }
                }
                1 => {
                    next_move = Some(match fen_part.chars().next().unwrap() {
                        'w' => White,
                        'b' => Black,
                        _ => panic!("Unexpected color for the next move!"),
                    });
                }
                2 => {
                    castling = EnumSet::new();
                    for castle_right in fen_part.chars() {
                        match castle_right {
                            'Q' => castling |= WhiteQueenSide,
                            'K' => castling |= WhiteKingSide,
                            'q' => castling |= BlackQueenSide,
                            'k' => castling |= BlackKingSide,
                            '-' => {}
                            _ => panic!("Incorrect castling details"),
                        }
                    }
                }
                3 => {
                    let mut fen_chars = fen_part.chars();
                    let first_char = fen_chars.next().unwrap();
                    if first_char != '-' {
                        ep = Some(BoardPos(
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
            score: raw.score(),
            board: raw,
            who_moves: if let Some(mover) = next_move {
                mover
            } else {
                panic!("Unspecified whose turn it is!")
            },
            castling,
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

    /// Allows exporting a `PSBoard` to fen for external analysis
    /// <https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation>
    /// # Panics
    /// When `PieceState` is failing to produce a proper output
    pub fn to_fen(&self) -> String {
        let mut ret = String::new();
        let mut since_piece: u8;
        for row in (0..8).rev() {
            since_piece = 0;
            for col in 0..8 {
                if let Some(ps) = self.board[BoardPos(row, col)] {
                    if since_piece != 0 {
                        ret.push((since_piece + b'0') as char);
                    }
                    ret.push(format!("{ps}").pop().unwrap());
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
        if self.castling.is_empty() {
            ret.push('-');
        } else {
            static CASTLING_ORDER: &str = "KQkq";
            static CASTLING_MASKS: [Castling; 4] =
                [WhiteKingSide, WhiteQueenSide, BlackKingSide, BlackQueenSide];
            ret.push_str(
                CASTLING_ORDER
                    .chars()
                    .zip(CASTLING_MASKS.iter())
                    .filter_map(|(sym, mask)| {
                        if self.castling.contains(*mask) {
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
        if let Some(BoardPos(row, col)) = &self.ep {
            ret.push((*col as u8 + b'a') as char);
            ret.push((*row as u8 + b'1') as char);
        } else {
            ret.push('-');
        }
        ret.push_str(format!(" {} {}", self.half_moves_since_pawn, self.move_count).as_str());
        ret
    }
}
