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

use crate::baserules::board_rep::BaseMove;
use crate::baserules::piece_color::PieceColor::{Black, White};
use crate::baserules::piece_kind::PieceKind::{Bishop, King, Knight, Pawn, Queen, Rook};
use crate::baserules::piece_state::PieceState;
use crate::baserules::positions::AbsoluteBoardPos;
use crate::util::{IntResult, TryWithPanic};
use lazy_static::lazy_static;
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

    /// Determines what piece is at a particular location of the board
    ///
    /// # Example use:
    /// ```
    /// use dbce::baserules::piece_color::PieceColor::White;
    /// use dbce::baserules::piece_kind::PieceKind::King;
    /// use dbce::baserules::piece_state::PieceState;
    /// use dbce::baserules::positions::AbsoluteBoardPos;
    /// use dbce::baserules::rawboard::RawBoard;
    /// use dbce::util::TryWithPanic;
    ///
    /// let board = RawBoard::default();
    /// let king_pos:AbsoluteBoardPos = "e1".transform();
    /// let king = board[king_pos];
    /// assert_eq!(Some(PieceState { kind: King, color: White }),king)
    /// ```
    #[inline]
    pub fn get_loc(&self, pos: AbsoluteBoardPos) -> &Option<PieceState> {
        &self[pos]
    }

    /// Allows write access to the raw board, transforms PieceState changes to the bit level representation of the board.
    ///
    /// # Example use:
    /// ```
    /// use dbce::baserules::piece_state::PieceState;
    /// use dbce::baserules::rawboard::RawBoard;
    /// use dbce::util::TryWithPanic;
    /// let mut empty_board = RawBoard::empty();
    /// let white_rook = Some('R'.transform());
    /// let a1 = "a1".transform();
    /// empty_board.set_loc(a1, &white_rook);
    /// assert_eq!(white_rook, empty_board[a1]);
    /// ```
    #[inline]
    pub fn set_loc(
        &mut self,
        AbsoluteBoardPos(row, col): AbsoluteBoardPos,
        piece: &Option<PieceState>,
    ) {
        let (shift_amount, piece_mask, us_row) = Self::shift_and_mask(row, col);
        let a_row = self.0[us_row] & piece_mask;
        let a_bit_piece = PieceState::bits_u32(piece) << shift_amount;
        self.0[us_row] = a_row | a_bit_piece;
    }

    /// Helper method to get the row and column information in forms usable by location based methods
    #[inline]
    fn shift_and_mask(row: u8, col: u8) -> (u8, u32, usize) {
        let shift_amount = col << 2;
        let piece_mask = !(0b1111 << shift_amount);
        (shift_amount, piece_mask, row as usize)
    }

    #[inline]
    pub fn clear_loc(&mut self, AbsoluteBoardPos(row, col): AbsoluteBoardPos) {
        let (_, piece_mask, us_row) = Self::shift_and_mask(row, col);
        self.0[us_row] &= piece_mask;
    }

    /// Makes a rudimentary move, sets the new loc with the piece given and erases the previous.
    #[inline]
    pub(crate) fn make_move_with(&mut self, the_move: &BaseMove, piece: &Option<PieceState>) {
        self.set_loc(the_move.to, piece);
        self.clear_loc(the_move.from);
    }

    /// A simple scoring mechanism which just counts up the pieces and pawns based on their usual values
    ///
    /// # Example use
    /// Each `PSBoard` has its score automatically calculated with this method during creation, so this is an indirect demonstration.
    /// ```
    /// use dbce::baserules::board::PSBoard;
    /// use dbce::baserules::rawboard::{MATE};
    /// let scholars_mate = PSBoard::from_fen("1rbqQb1r/pppp2pp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b QKqk - 9 5").unwrap();
    /// assert_eq!(MATE, scholars_mate.raw.score());
    /// ```
    pub async fn score(&self) -> f32 {
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

    pub fn from_fen_prefix(fen: impl AsRef<str>) -> IntResult<Self> {
        let mut new_board = RawBoard::empty();
        let fen_part = fen.as_ref();
        let mut row = 7;
        let mut col = 0;
        for piece_info in fen_part.chars() {
            if piece_info.is_ascii_digit() {
                col += piece_info as u8 - b'0';
            } else if piece_info == '/' {
                col = 0;
                assert_ne!(row, 0, "Should not have more rows on the chessboard!");
                row -= 1;
            } else {
                new_board.set_loc((row, col).transform(), &Some(piece_info.try_into()?));
                col += 1;
            }
        }
        Ok(new_board)
    }

    pub fn to_fen_prefix(&self) -> String {
        let mut ret = String::new();
        let mut since_piece: u8;
        for row in (0..8u8).rev() {
            since_piece = 0;
            for col in 0..8 {
                if let Some(ps) = self[(row, col)] {
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
        ret
    }
}

lazy_static! {
    static ref STARTING_POSITION_BOARD: RawBoard = {
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
                    (row, col).transform(),
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
    };
}

impl Default for RawBoard {
    /// Gets the starting position into the raw board.
    ///
    /// #Example use:
    /// ```
    /// use dbce::baserules::piece_state::PieceState;
    /// use dbce::baserules::rawboard::RawBoard;
    /// use dbce::util::TryWithPanic;
    /// let starting_position = RawBoard::default();
    /// assert_eq!(Some('R'.transform()), starting_position[(0,0)]);
    /// assert_eq!(None, starting_position[(3,0)]);
    /// ```
    fn default() -> Self {
        *STARTING_POSITION_BOARD
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
        for row in (0..8u8).rev() {
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
                    &self[(row, col)]
                        .map_or('-', |ps| format!("{}", ps.to_unicode()).pop().unwrap()),
                    suffix
                ));
            }
            return_string.push('\n');
        }
        return_string.push_str(COL);
        write!(f, "{return_string}")
    }
}

impl ops::Index<AbsoluteBoardPos> for RawBoard {
    type Output = Option<PieceState>;

    /// Provides access into the raw board with simple board position coordinates in the index operator.
    ///
    /// # Example use
    /// ```
    /// use std::str::FromStr;
    /// use dbce::baserules::piece_state::PieceState;
    /// use dbce::baserules::positions::AbsoluteBoardPos;
    /// use dbce::baserules::rawboard::RawBoard;
    /// use dbce::util::TryWithPanic;
    /// let starting_position = RawBoard::default();
    /// let black_king = starting_position[AbsoluteBoardPos(0,4)];
    /// assert_eq!(Some('K'.transform()),black_king);
    /// ```
    #[inline]
    fn index(&self, AbsoluteBoardPos(row, col): AbsoluteBoardPos) -> &Self::Output {
        PieceState::masked_ps_conversion(col as usize, self.0[row as usize])
    }
}

impl ops::Index<&str> for RawBoard {
    type Output = Option<PieceState>;

    /// Provides access into the raw board with simple uci position coordinates in the index operator.
    ///
    /// # Example use
    /// ```
    /// use std::str::FromStr;
    /// use dbce::baserules::piece_state::PieceState;
    /// use dbce::baserules::rawboard::RawBoard;
    /// use dbce::util::TryWithPanic;
    /// let starting_position = RawBoard::default();
    /// let black_king = starting_position["e8"];
    /// assert_eq!(Some('k'.transform()),black_king);
    /// ```
    #[inline]
    fn index(&self, uci: &str) -> &Self::Output {
        let abs_pos: AbsoluteBoardPos = uci.transform();
        &self[abs_pos]
    }
}

impl ops::Index<(u8, u8)> for RawBoard {
    type Output = Option<PieceState>;

    /// Provides access into the raw board with simple uci position coordinates in the index operator.
    ///
    /// # Example use
    /// ```
    /// use std::str::FromStr;
    /// use dbce::baserules::piece_state::PieceState;
    /// use dbce::baserules::rawboard::RawBoard;
    /// use dbce::util::TryWithPanic;
    /// let starting_position = RawBoard::default();
    /// let black_king = starting_position[(0,0)];
    /// assert_eq!(Some('R'.transform()),black_king);
    /// ```
    #[inline]
    fn index(&self, raw_pos: (u8, u8)) -> &Self::Output {
        let abs_pos: AbsoluteBoardPos = raw_pos.transform();
        &self[abs_pos]
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
    /// use dbce::util::TryWithPanic;
    /// let starting_position = RawBoard::default();
    /// let black_bishop = Some('b'.transform());
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
    use crate::baserules::rawboard::MATE;
    use crate::baserules::{board::PSBoard, board_rep::BaseMove};
    use tokio::test;

    #[test]
    async fn iterator_test() {
        let psboard = PSBoard::default();
        let mut iter = psboard.raw.into_iter();
        assert_eq!(&psboard["a1"], iter.next().unwrap());
        let mut iter = psboard.raw.into_iter();
        assert_eq!(&psboard["d8"], iter.nth(8 * 7 + 3).unwrap());
    }

    #[test]
    async fn test_mate() {
        let board: PSBoard =
            PSBoard::from_fen("rnb1kbnr/pppp1ppp/4p3/8/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3")
                .await
                .unwrap();

        let score = board.score().await;
        let piece_move: BaseMove = "b1c3".try_into().unwrap();
        let impossible_board = board.make_move_noncached(&piece_move.into()).await;

        let piece_move_impossible: BaseMove = "h4e1".try_into().unwrap();
        let mate_board = impossible_board
            .make_move_noncached(&piece_move_impossible.into())
            .await;

        let mate_score = mate_board.score().await;
        assert_eq!(
            mate_score,
            impossible_board.who_moves.mate_multiplier() * MATE
        );
        assert_eq!(score, 0.0);
    }
}
