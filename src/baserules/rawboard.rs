/*
 *  ========================================================================
 *  DBCE chess bot, plain board representation
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
use crate::baserules::piece_color::PieceColor::{Black, White};
use crate::baserules::piece_kind::PieceKind::{Bishop, King, Knight, Pawn, Queen, Rook};
use crate::baserules::piece_state::PieceState;
use std::fmt::{Display, Formatter};
use std::ops;

pub static MATE: f32 = 1000.0;

pub fn is_mate(score: f32) -> bool {
    (score.abs() - MATE).abs() < 50.0
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct RawBoard([u32; 8]);

impl RawBoard {
    /// Creates a brand new raw board with no pieces on it
    ///
    /// # Example use:
    /// ```
    /// use dbce::baserules::rawboard::RawBoard;
    /// let empty_board = RawBoard::empty();
    /// assert!(empty_board.into_iter().find(|ps| ps.is_some()).is_none())
    /// ```
    #[inline]
    pub fn empty() -> Self {
        RawBoard([0; 8])
    }

    /// Allows write access to the raw board, transforms PieceState changes to the bit level representation of the board.
    ///
    /// # Example use:
    /// ```
    /// use dbce::baserules::board_rep::BoardPos;
    /// use dbce::baserules::piece_state::PieceState;
    /// use dbce::baserules::rawboard::RawBoard;
    /// let mut empty_board = RawBoard::empty();
    /// let white_rook = Some(PieceState::from_char('R'));
    /// let a1:BoardPos = "a1".try_into().unwrap();
    /// empty_board.set_loc(a1, &white_rook);
    /// assert_eq!(white_rook, empty_board[a1]);
    /// ```
    #[inline]
    pub fn set_loc(&mut self, BoardPos(row, col): BoardPos, piece: &Option<PieceState>) {
        let shift_amount = col << 2;
        let piece_mask = !(0b1111 << shift_amount);
        let us_row = row as usize;
        let a_row = self.0[us_row] & piece_mask;
        let a_bit_piece = (PieceState::bits(piece) as u32) << shift_amount;
        self.0[us_row] = a_row | a_bit_piece;
    }

    /// A simple scoring mechanism which just counts up the pieces and pawns based on their usual values
    ///
    /// # Example use
    /// Each `PSBoard` has its score automatically calculated with this method during creation, so this is an indirect demonstration.
    /// ```
    /// use dbce::baserules::board::PSBoard;
    /// use dbce::baserules::rawboard::{MATE};
    /// let scholars_mate = PSBoard::from_fen("1rbqQb1r/pppp2pp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b QKqk - 9 5");
    /// assert_eq!(MATE, scholars_mate.raw.score());
    /// ```
    pub fn score(&self) -> f32 {
        let (loc_score, white_king_found, black_king_found) = self
            .into_iter()
            .filter_map(|c_p| *c_p)
            .map(|curr_piece| match (curr_piece.kind, curr_piece.color) {
                (Pawn, White) => (1f32, false, false),
                (Pawn, Black) => (-1f32, false, false),
                (Knight, White) => (3f32, false, false),
                (Knight, Black) => (-3f32, false, false),
                (Bishop, White) => (3.1f32, false, false),
                (Bishop, Black) => (-3.1f32, false, false),
                (Rook, White) => (5f32, false, false),
                (Rook, Black) => (-5f32, false, false),
                (Queen, White) => (9f32, false, false),
                (Queen, Black) => (-9f32, false, false),
                (King, White) => (0f32, true, false),
                (King, Black) => (0f32, false, true),
            })
            .fold(
                (0f32, false, false),
                |(curr_score, curr_white_king, curr_black_king),
                 (score_adjust, white_king, black_king)| {
                    (
                        curr_score + score_adjust,
                        curr_white_king | white_king,
                        curr_black_king | black_king,
                    )
                },
            );
        if white_king_found {
            if black_king_found {
                loc_score
            } else {
                MATE
            }
        } else {
            -MATE
        }
    }
}

impl Default for RawBoard {
    /// Gets the starting position into the raw board.
    ///
    /// #Example use:
    /// ```
    /// use dbce::baserules::board_rep::BoardPos;
    /// use dbce::baserules::piece_state::PieceState;
    /// use dbce::baserules::rawboard::RawBoard;
    /// let starting_position = RawBoard::default();
    /// assert_eq!(Some(PieceState::from_char('R')), starting_position[(0,0).into()]);
    /// assert_eq!(None, starting_position[(3,0).into()]);
    /// ```
    fn default() -> Self {
        let mut raw = RawBoard::empty();
        for row in 0..8 {
            let (c, only_pawn) = match row {
                0 => (White, None),
                1 => (White, Some(Pawn)),
                6 => (Black, Some(Pawn)),
                7 => (Black, None),
                _ => continue,
            };
            for col in 0..8 {
                raw.set_loc(
                    (row, col).into(),
                    &Some(PieceState {
                        kind: if only_pawn.is_some() {
                            Pawn
                        } else {
                            match col {
                                0 | 7 => Rook,
                                1 | 6 => Knight,
                                2 | 5 => Bishop,
                                3 => Queen,
                                4 => King,
                                _ => panic!("Impossible"),
                            }
                        },
                        color: c,
                    }),
                );
            }
        }
        raw
    }
}

impl Display for RawBoard {
    /// This allows a simple text display of the raw board on your console. Good for debugging purposes
    ///
    /// # Example:
    /// ```
    /// use dbce::baserules::board::PSBoard;
    /// use dbce::baserules::rawboard::RawBoard;
    /// let empty_board = RawBoard::empty();
    /// let formatted = format!("{empty_board}");
    /// assert_eq!("  abcdefgh\n8 -------- 8\n7 -------- 7\n6 -------- 6\n5 -------- 5\n4 -------- 4\n3 -------- 3\n2 -------- 2\n1 -------- 1\n  abcdefgh\n",formatted);
    /// ```
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        static COL: &str = "  abcdefgh\n";
        let mut return_string = String::new();
        return_string.push_str(COL);
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
                    if let Some(ps) = &self[(row, col).into()] {
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

impl ops::Index<BoardPos> for RawBoard {
    type Output = Option<PieceState>;

    /// Provides access into the raw board with simple board position coordinates in the index operator.
    ///
    /// # Example use
    /// ```
    /// use std::str::FromStr;
    /// use dbce::baserules::board_rep::BoardPos;
    /// use dbce::baserules::piece_state::PieceState;
    /// use dbce::baserules::rawboard::RawBoard;
    /// let starting_position = RawBoard::default();
    /// let black_king = starting_position["e8".try_into().unwrap()];
    /// assert_eq!(Some(PieceState::from_char('k')),black_king);
    /// ```
    #[inline]
    fn index(&self, BoardPos(row, col): BoardPos) -> &Self::Output {
        PieceState::masked_ps_conversion(col as usize, self.0[row as usize])
    }
}

impl<'a> IntoIterator for &'a RawBoard {
    type Item = &'a Option<PieceState>;
    type IntoIter = RawBoardIterator<'a>;

    /// Allows easy traversal of the whole board in with a single iterator
    /// #Example use:
    /// ```
    /// use dbce::baserules::piece_state::PieceState;
    /// use dbce::baserules::rawboard::RawBoard;
    /// let starting_position = RawBoard::default();
    /// let black_bishop = Some(PieceState::from_char('b'));
    /// assert!(starting_position.into_iter().any(|ps| ps==&black_bishop));
    /// ```
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        RawBoardIterator {
            raw_board: self,
            curr_idx: 0,
        }
    }
}

/// Represents the iterator state for the rawboard while we are traversing through a board
/// Should be created with RawBoard::into_iter.
pub struct RawBoardIterator<'a> {
    raw_board: &'a RawBoard,
    curr_idx: usize,
}

impl<'a> Iterator for RawBoardIterator<'a> {
    type Item = &'a Option<PieceState>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.curr_idx == 64 {
            None
        } else {
            let ret = PieceState::masked_ps_conversion(self.curr_idx & 0b111, unsafe {
                *self.raw_board.0.get_unchecked(self.curr_idx >> 3)
            });
            self.curr_idx += 1;
            Some(ret)
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remainder = 64 - self.curr_idx;
        (remainder, Some(remainder))
    }
}

impl<'a> ExactSizeIterator for RawBoardIterator<'a> {}

#[cfg(test)]
mod test {
    use crate::baserules::board::PSBoard;

    #[test]
    fn iterator_test() {
        let psboard = PSBoard::default();
        let mut iter = psboard.raw.into_iter();
        assert_eq!(
            psboard.get_loc("a1".try_into().unwrap()),
            iter.next().unwrap()
        );
        let mut iter = psboard.raw.into_iter();
        assert_eq!(
            psboard.get_loc("d8".try_into().unwrap()),
            iter.nth(8 * 7 + 3).unwrap()
        );
    }
}
