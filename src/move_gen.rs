/*
 *  ========================================================================
 *  DBCE chess bot, movement generation
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
use crate::board_rep::PieceKind::{Bishop, King, Knight, Pawn, Queen, Rook};
use crate::board_rep::{BaseMove, PieceKind, PossibleMove};
use std::cell::RefCell;
use std::cmp::{max, min};

impl PSBoard {
    fn switch_sides(&self) -> PSBoard {
        let mut other_side = *self;
        other_side.who_moves = if self.who_moves == White {
            Black
        } else {
            White
        };
        other_side
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
    ) -> Option<(PossibleMove, f32)>
    where
        I: Fn(&Self, &(i8, i8)) -> bool,
    {
        if loc.0 & 7i8 == loc.0 && loc.1 & 7i8 == loc.1 &&
            // Move is on the board
            on_board_rule(self, &loc)
        {
            // Move is allowed by the rule, we generate it
            Some((
                PossibleMove {
                    themove: BaseMove {
                        from: (row, col),
                        to: (loc.0, loc.1),
                    },
                    pawnpromotion: None,
                    rook: None,
                },
                0f32,
            ))
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
        out: &mut Vec<(PossibleMove, f32)>,
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
        out: &mut Vec<(PossibleMove, f32)>,
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
    fn gen_moves_from_vecs<'a, I>(
        &self,
        row: i8,
        col: i8,
        vecs: I,
        out: &mut Vec<(PossibleMove, f32)>,
    ) where
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
    pub fn gen_potential_moves(
        &self,
        castleallowed: bool,
        themoves: &mut Vec<(PossibleMove, f32)>,
    ) {
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
                                                                m.0.themove.to.0 == piecerow
                                                                    && m.0.themove.to.1 <= maxcol
                                                                    && m.0.themove.to.1 >= mincol
                                                            })
                                                            .count()
                                                    };
                                                    if count == 0 {
                                                        // would not cross check
                                                        themoves.push((
                                                            PossibleMove {
                                                                themove: BaseMove {
                                                                    from: (row, col),
                                                                    to: (
                                                                        row,
                                                                        CASTLING_MOVES[idx][0],
                                                                    ),
                                                                },
                                                                pawnpromotion: None,
                                                                rook: Some(BaseMove {
                                                                    from: (
                                                                        row,
                                                                        CASTLING_MOVES[idx][1],
                                                                    ),
                                                                    to: (
                                                                        row,
                                                                        CASTLING_MOVES[idx][2],
                                                                    ),
                                                                }),
                                                            },
                                                            0f32,
                                                        ));
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
                                if themoves[prelen].0.themove.to.0 == 0
                                    || themoves[prelen].0.themove.to.0 == 7
                                {
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
                                            themoves.push((
                                                PossibleMove {
                                                    themove: BaseMove {
                                                        from: m.0.themove.from,
                                                        to: m.0.themove.to,
                                                    },
                                                    pawnpromotion: Some(pk),
                                                    rook: None,
                                                },
                                                0f32,
                                            ));
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
}
