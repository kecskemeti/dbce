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
use crate::baserules::piece_state::PieceState;
use std::ops;

#[derive(Clone, Copy)]
pub struct RawBoard([u32; 8]);

impl RawBoard {
    pub fn empty() -> Self {
        RawBoard([0; 8])
    }
    #[inline]
    pub fn set_loc(&mut self, BoardPos(row, col): BoardPos, piece: &Option<PieceState>) {
        let shift_amount = col << 2;
        let piece_mask = !(0b1111 << shift_amount);
        let us_row = row as usize;
        let a_row = self.0[us_row] & piece_mask;
        let a_bit_piece = (PieceState::bits(piece) as u32) << shift_amount;
        self.0[us_row] = a_row | a_bit_piece;
    }
}

impl ops::Index<BoardPos> for RawBoard {
    type Output = Option<PieceState>;

    #[inline]
    fn index(&self, BoardPos(row, col): BoardPos) -> &Self::Output {
        let a_row = self.0[row as usize];
        let a_bit_piece = a_row >> (col << 2) & 0b1111;
        PieceState::from_u8(a_bit_piece as u8)
    }
}
impl<'a> IntoIterator for &'a RawBoard {
    type Item = &'a Option<PieceState>;
    type IntoIter = RawBoardIterator<'a>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        RawBoardIterator {
            raw_board: self,
            curr_row_data: self.0[0],
            curr_row: 0,
            curr_col: 0,
        }
    }
}

pub struct RawBoardIterator<'a> {
    raw_board: &'a RawBoard,
    curr_row_data: u32,
    curr_row: usize,
    curr_col: usize,
}

impl<'a> Iterator for RawBoardIterator<'a> {
    type Item = &'a Option<PieceState>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.curr_col == 8 {
            self.curr_row += 1;
            if self.curr_row == 8 {
                return None;
            }
            self.curr_row_data = self.raw_board.0[self.curr_row];
            self.curr_col = 0;
        }
        let ret = PieceState::from_u8((self.curr_row_data & 0b1111) as u8);
        self.curr_col += 1;
        self.curr_row_data >>= 4;
        Some(ret)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remainder = 64 - self.curr_row * 8 - self.curr_col;
        (remainder, Some(remainder))
    }
}

impl<'a> ExactSizeIterator for RawBoardIterator<'a> {}

#[cfg(test)]
mod test {
    use crate::baserules::board::PSBoard;
    use crate::baserules::board_rep::BoardPos;
    use std::str::FromStr;

    #[test]
    fn iterator_test() {
        let psboard = PSBoard::default();
        let mut iter = psboard.board.into_iter();
        assert_eq!(
            psboard.get_loc(BoardPos::from_str("a1").unwrap()),
            iter.next().unwrap()
        );
        let mut iter = psboard.board.into_iter();
        assert_eq!(
            psboard.get_loc(BoardPos::from_str("d8").unwrap()),
            iter.nth(8 * 7 + 3).unwrap()
        );
    }
}
