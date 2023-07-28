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
use crate::baserules::board::PSBoard;
use crate::baserules::castling::Castling;
use crate::baserules::move_gen::{CASTLE_ALLOWED, CASTLE_FORBIDDEN};
use crate::baserules::piece_color::PieceColor;
use crate::baserules::piece_color::PieceColor::*;
use crate::baserules::piece_kind::PieceKind::*;
use crate::baserules::piece_state::PieceState;
use crate::baserules::rawboard::RawBoard;
use crate::util::{IntResult, TryWithPanic};
use enumset::EnumSet;
use std::fmt::{Display, Formatter};

impl PieceState {
    pub fn from_unicode(unicode_char: char) -> PieceState {
        match unicode_char {
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
        }
        .transform()
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
    /// let immortal_game = PSBoard::from_fen("r1bk3r/p2pBpNp/n4n2/1p1NP2P/6P1/3P4/P1P1K3/q5b1 b - - 0 1").unwrap();
    /// let formatted = format!("{immortal_game}");
    /// assert_eq!("Black to move, move 1\n  abcdefgh\n8 ♜-♝♚---♜ 8\n7 ♟--♟♗♟♘♟ 7\n6 ♞----♞-- 6\n5 -♟-♘♙--♙ 5\n4 ------♙- 4\n3 ---♙---- 3\n2 ♙-♙-♔--- 2\n1 ♛-----♝- 1\n  abcdefgh\n",formatted);
    /// ```
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?} to move, move {}\n{}",
            self.who_moves, self.move_count, self.raw
        )
    }
}

impl PSBoard {
    /// Allows initialising a particular position from fen
    /// <https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation>
    /// # Panics
    /// When the input string is an incorrect fen
    pub fn from_fen(fen: &str) -> IntResult<Self> {
        let mut raw = RawBoard::empty();
        let mut next_move = None;
        let mut castling = EnumSet::empty();
        let mut ep = None;
        let mut half: Option<u16> = None;
        let mut full: Option<u16> = None;
        for (idx, fen_part) in fen.split_whitespace().enumerate() {
            match idx {
                0 => raw = RawBoard::from_fen_prefix(fen_part)?,

                1 => {
                    let deciphered_who_moves: PieceColor =
                        fen_part.chars().next().unwrap().transform();
                    next_move = Some(deciphered_who_moves);
                }

                2 => {
                    castling = EnumSet::new();
                    for castle_right in fen_part.chars().filter(|c| *c != '-') {
                        let additional_castling: Castling = castle_right.transform();
                        castling |= additional_castling;
                    }
                }

                3 => {
                    if !fen_part.starts_with('-') {
                        ep = Some(fen_part.transform());
                    }
                }

                4 => half = Some(fen_part.parse().unwrap()),

                5 => full = Some(fen_part.parse().unwrap()),

                _ => panic!("Too many fields in the FEN"),
            }
        }
        Ok(PSBoard {
            score: raw.score(),
            raw,
            who_moves: next_move.unwrap_or_else(|| panic!("Unspecified whose turn it is!")),
            king_move_gen: if castling.is_empty() {
                &CASTLE_FORBIDDEN
            } else {
                &CASTLE_ALLOWED
            },
            castling,
            ep,
            move_count: full.unwrap_or_else(|| panic!("Unspecified move count")),
            half_moves_since_pawn: half.unwrap_or_else(|| panic!("Unspecified half move count")),
        })
    }

    /// Allows exporting a `PSBoard` to fen for external analysis
    /// <https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation>
    /// # Panics
    /// When `PieceState` is failing to produce a proper output
    pub fn to_fen(&self) -> String {
        let mut ret = self.raw.to_fen_prefix();
        ret.push(' ');
        ret.push(self.who_moves.fen_color());
        ret.push(' ');
        if self.castling.is_empty() {
            ret.push('-');
        } else {
            let mut castling: Vec<char> = self
                .castling
                .iter()
                .map(|a_castling| a_castling.fen_char())
                .collect();
            castling.sort();
            ret.push_str(castling.into_iter().collect::<String>().as_str())
        }
        ret.push(' ');
        ret.push_str(self.ep.map_or("-".into(), |p| p.to_string()).as_str());
        ret.push_str(format!(" {} {}", self.half_moves_since_pawn, self.move_count).as_str());
        ret
    }
}
