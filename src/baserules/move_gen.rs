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
 *  (C) Copyright 2022-3, Gabor Kecskemeti
 */

use crate::baserules::board::PSBoard;
use crate::baserules::board_rep::{BaseMove, PossibleMove};
use crate::baserules::castling::{kingside_castle, queenside_castle, Castling};
use crate::baserules::piece_kind::PieceKind;
use crate::baserules::piece_kind::PieceKind::*;
use crate::baserules::positions::{AbsoluteBoardPos, RelativeBoardPos};
use crate::util::TryWithPanic;
use enumset::EnumSet;
use lazy_static::lazy_static;
use std::cell::RefCell;
use std::cmp::{max, min};

pub(crate) trait KingMove: Sync {
    fn gen_king_moves(
        &self,
        board: &PSBoard,
        ab: AbsoluteBoardPos,
        the_moves: &mut Vec<PossibleMove>,
    );
}

pub struct Castle(pub &'static NotCastle);

pub static CASTLE_ALLOWED: Castle = Castle(&CASTLE_FORBIDDEN);

impl KingMove for Castle {
    fn gen_king_moves(
        &self,
        board: &PSBoard,
        ab: AbsoluteBoardPos,
        the_moves: &mut Vec<PossibleMove>,
    ) {
        self.0.gen_king_moves(board, ab, the_moves);
        let AbsoluteBoardPos(row, col) = ab;
        // Castling:
        if ab == board.who_moves.starting_king_pos() {
            // the king is in its original location we need a more in depth check on castling
            if !board.castling.is_disjoint(board.who_moves.all_castling()) {
                // there are castling opportunities
                static CASTLING_SIDE: [EnumSet<Castling>; 2] =
                    [queenside_castle(), kingside_castle()];
                static CASTLING_RANGES: [(u8, u8); 2] = [(1, 3), (5, 6)];
                static CASTLING_MOVES: [[u8; 3]; 2] = [[2, 0, 3], [6, 7, 5]];
                for (idx, side) in CASTLING_SIDE.iter().enumerate() {
                    // castling is still allowed for the particular side &&
                    // castling range is free of chess pieces
                    if !board.castling.is_disjoint(*side)
                        && (CASTLING_RANGES[idx].0..=CASTLING_RANGES[idx].1)
                            .all(|in_between_col| board[(row, in_between_col)].is_none())
                    {
                        // Let's see if we would cross a check
                        let otherside = board.switch_sides();
                        let mincol = min(CASTLING_MOVES[idx][0], 4);
                        let maxcol = max(CASTLING_MOVES[idx][0], 4);
                        let crosses_check = {
                            let mut other_side_moves = Vec::new();
                            {
                                otherside.gen_potential_moves(&mut other_side_moves);
                            }
                            other_side_moves.iter().any(|m| {
                                m.the_move.to.0 == board.who_moves.piece_row()
                                    && m.the_move.to.1 <= maxcol
                                    && m.the_move.to.1 >= mincol
                            })
                        };
                        if !crosses_check {
                            // would not cross check
                            the_moves.push(PossibleMove {
                                the_move: BaseMove::move_in_row(row, col, CASTLING_MOVES[idx][0]),
                                pawn_promotion: None,
                                rook: Some(BaseMove::move_in_row(
                                    row,
                                    CASTLING_MOVES[idx][1],
                                    CASTLING_MOVES[idx][2],
                                )),
                            });
                        }
                    }
                }
            }
        }
    }
}

pub struct NotCastle;
pub static CASTLE_FORBIDDEN: NotCastle = NotCastle;

impl KingMove for NotCastle {
    fn gen_king_moves(
        &self,
        board: &PSBoard,
        position: AbsoluteBoardPos,
        the_moves: &mut Vec<PossibleMove>,
    ) {
        board.gen_moves_from_dirs(
            position,
            &PSBoard::piece_move_rule,
            King.vec_moves(),
            the_moves,
        );
    }
}

impl PSBoard {
    /// does a dirty side switch to allow seeing castling issues
    fn switch_sides(&self) -> PSBoard {
        PSBoard {
            who_moves: self.who_moves.invert(),
            king_move_gen: &CASTLE_FORBIDDEN,
            ..*self
        }
    }

    /// Pieces can move and take till they hit another piece, assuming it is not the same colour
    #[inline]
    fn piece_move_rule(&self, pos: AbsoluteBoardPos) -> bool {
        self[pos]
            .as_ref()
            .map_or(true, |other_piece| other_piece.color != self.who_moves)
    }

    /// Pawns can only move to empty spaces, cannot take in forward movement
    #[inline]
    fn pawn_move_rule(&self, pos: AbsoluteBoardPos) -> bool {
        self[pos].is_none()
    }

    /// Pawns can take in diagonals, even with en passant
    #[inline]
    fn pawn_take_rule(&self, pos: AbsoluteBoardPos) -> bool {
        self[pos].as_ref().map_or_else(
            || {
                // En passant
                self.ep.as_ref().map_or(false, |ep_loc| &pos == ep_loc)
            },
            |other_piece| {
                // regular move
                other_piece.color != self.who_moves
            },
        )
    }

    /// Determines if a particular move is allowed based on the piece movement rule and the coordinate validity
    /// if so it produces a new move
    #[inline]
    fn fil_map_core<I>(
        &self,
        pos: AbsoluteBoardPos,
        loc: AbsoluteBoardPos,
        on_board_rule: &I,
    ) -> Option<PossibleMove>
    where
        I: Fn(&Self, AbsoluteBoardPos) -> bool,
    {
        if
        // Move is on the board
        on_board_rule(self, loc) {
            // Move is allowed by the rule, we generate it
            Some(PossibleMove::simple_move(BaseMove::from_two_pos(pos, loc)))
        } else {
            None
        }
    }

    /// Generates a set of valid moves based on the coordinate adjustments passed in via the iterator
    /// Immediately deposits the moves in the output vector
    #[inline]
    fn gen_moves_from_dirs<'a, I: IntoIterator<Item = &'a RelativeBoardPos>, J>(
        &self,
        position: AbsoluteBoardPos,
        on_board_rule: &J,
        possible_moves: I,
        out: &mut Vec<PossibleMove>,
    ) where
        J: Fn(&Self, AbsoluteBoardPos) -> bool,
    {
        out.extend(
            possible_moves
                .into_iter()
                .filter_map(|m| position.fallible_add(*m).ok())
                .filter_map(|new_loc| self.fil_map_core(position, new_loc, on_board_rule)),
        );
    }
    //falible add

    /// This does the same as `gen_moves_from_dirs`, but stops at the first occasion
    /// when moves are no longer possible
    #[inline]
    fn gen_moves_from_dirs_with_stop<'a, I, J>(
        &self,
        position: AbsoluteBoardPos,
        on_board_rule: &J,
        possible_moves: I,
        out: &mut Vec<PossibleMove>,
    ) where
        I: IntoIterator<Item = &'a RelativeBoardPos>,
        J: Fn(&Self, AbsoluteBoardPos) -> bool,
    {
        out.extend(
            possible_moves
                .into_iter()
                .map_while(|m| position.fallible_add(*m).ok())
                .map_while(|new_loc| self.fil_map_core(position, new_loc, on_board_rule)),
        );
    }

    /// This generates moves based on directional vectors (useful for rooks, bishops and queens)
    fn gen_moves_from_vecs<'a, I: IntoIterator<Item = &'a RelativeBoardPos>>(
        &self,
        position: AbsoluteBoardPos,
        vecs: I,
        out: &mut Vec<PossibleMove>,
    ) {
        // transform vec rel board
        for RelativeBoardPos(x, y) in vecs {
            lazy_static! { static ref DIRECTIONAL_MOVES: [Vec<RelativeBoardPos>; 9] = [
                // this array is laid out so it is easy to map into it with the below formula using just the input coords
                RelativeBoardPos::transform_more([
                    (-1, -1),
                    (-2, -2),
                    (-3, -3),
                    (-4, -4),
                    (-5, -5),
                    (-6, -6),
                    (-7, -7),
                ]),
                RelativeBoardPos::transform_more([
                    (-1, 0),
                    (-2, 0),
                    (-3, 0),
                    (-4, 0),
                    (-5, 0),
                    (-6, 0),
                    (-7, 0),
                ]),
                RelativeBoardPos::transform_more([
                    (-1, 1),
                    (-2, 2),
                    (-3, 3),
                    (-4, 4),
                    (-5, 5),
                    (-6, 6),
                    (-7, 7),
                ]),
                RelativeBoardPos::transform_more([
                    (0, -1),
                    (0, -2),
                    (0, -3),
                    (0, -4),
                    (0, -5),
                    (0, -6),
                    (0, -7),
                ]),
                RelativeBoardPos::transform_more([
                    // filler to make the x-y mapping easier
                    (0, 0)
                ]),
                RelativeBoardPos::transform_more([
                    (0, 1),
                    (0, 2),
                    (0, 3),
                    (0, 4),
                    (0, 5),
                    (0, 6),
                    (0, 7),
                ]),
                RelativeBoardPos::transform_more([
                    (1, -1),
                    (2, -2),
                    (3, -3),
                    (4, -4),
                    (5, -5),
                    (6, -6),
                    (7, -7),
                ]),
                RelativeBoardPos::transform_more([
                    (1, 0),
                    (2, 0),
                    (3, 0),
                    (4, 0),
                    (5, 0),
                    (6, 0),
                    (7, 0),
                ]),
                RelativeBoardPos::transform_more([
                    (1, 1),
                    (2, 2),
                    (3, 3),
                    (4, 4),
                    (5, 5),
                    (6, 6),
                    (7, 7),
                ]),
            ];}
            let a = &DIRECTIONAL_MOVES[(x + y + 2 * x + 4) as usize]; // the input coords directly map into the above array
            let allow_next = RefCell::new(true);
            self.gen_moves_from_dirs_with_stop(
                position,
                &move |s, m| {
                    if *allow_next.borrow() {
                        // Ensures we can take a piece but not go further or we need to stop a step before our own pieces
                        s[m].map_or(true, |trg| {
                            let mut d = allow_next.borrow_mut();
                            *d = false;
                            !(trg.color == s.who_moves)
                        })
                    } else {
                        false
                    }
                },
                a.iter(),
                out,
            );
        }
    }

    /// This figures out all the possible moves on the particular board
    ///
    /// # Example
    ///
    /// ```
    /// use dbce::baserules::board::PSBoard;
    /// use dbce::baserules::board_rep::BaseMove;
    /// let board = PSBoard::default();
    /// let mut moves = Vec::new();
    /// board.gen_potential_moves(&mut moves);
    /// let van_geet_opening = BaseMove::from_uci("b1c3").unwrap();
    /// let van_geet_opening_found = moves.iter().any(|m| m.the_move == van_geet_opening);
    /// assert!(van_geet_opening_found);
    /// ```
    pub fn gen_potential_moves(&self, the_moves: &mut Vec<PossibleMove>) {
        self.raw
            .into_iter()
            .enumerate()
            .filter_map(|(idx, ps)| {
                if let Some(curr) = ps {
                    if curr.color == self.who_moves {
                        return Some((
                            idx.transform(),
                            match curr.kind {
                                King => PSBoard::gen_king_moves,
                                Pawn => PSBoard::gen_pawn_moves,
                                Knight => PSBoard::gen_knight_moves,
                                Rook | Bishop | Queen => PSBoard::gen_vec_moves,
                            },
                        ));
                    }
                }
                None
            })
            .for_each(|(loc, call)| call(self, loc, the_moves));
    }

    fn gen_king_moves(&self, position: AbsoluteBoardPos, the_moves: &mut Vec<PossibleMove>) {
        self.king_move_gen.gen_king_moves(self, position, the_moves);
    }

    /// Pawn moves, takes and promotions
    fn gen_pawn_moves(&self, position: AbsoluteBoardPos, the_moves: &mut Vec<PossibleMove>) {
        // normal pawn move
        let prelen = the_moves.len();
        let pawn_move_now = if position.0 == self.who_moves.pawn_starting_row() {
            // two step pawn move at the beginning
            self.who_moves.pawn_double_step()
        } else {
            self.who_moves.pawn_single_step()
        };
        self.gen_moves_from_dirs_with_stop(
            position,
            &PSBoard::pawn_move_rule,
            pawn_move_now,
            the_moves,
        );

        // Pawn takes
        self.gen_moves_from_dirs(
            position,
            &PSBoard::pawn_take_rule,
            self.who_moves.pawn_takes_step(),
            the_moves,
        );
        if the_moves.len() > prelen {
            // Promotions handling
            if the_moves[prelen].the_move.to.0 == self.who_moves.pawn_promotion_row() {
                static PROMOTIONS: [PieceKind; 4] = [Rook, Knight, Queen, Bishop];
                let mut pawn_moves = [None, None, None];
                let mut idx = 0;
                while the_moves.len() != prelen {
                    pawn_moves[idx] = the_moves.pop();
                    idx += 1;
                }
                for m in pawn_moves.iter().flatten() {
                    for pk in PROMOTIONS {
                        the_moves.push(PossibleMove {
                            the_move: BaseMove {
                                from: m.the_move.from,
                                to: m.the_move.to,
                            },
                            pawn_promotion: Some(pk),
                            rook: None,
                        });
                    }
                }
            }
        }
    }

    /// The knight moves are not directional, so we handle them specifically
    fn gen_knight_moves(&self, position: AbsoluteBoardPos, the_moves: &mut Vec<PossibleMove>) {
        self.gen_moves_from_dirs(
            position,
            &PSBoard::piece_move_rule,
            Knight.vec_moves(),
            the_moves,
        );
    }

    /// All other pieces have simple directional movement, so we just use their directions to generate their possible moves
    fn gen_vec_moves(&self, position: AbsoluteBoardPos, the_moves: &mut Vec<PossibleMove>) {
        self.gen_moves_from_vecs(
            position,
            self.get_loc(position).unwrap().kind.vec_moves(),
            the_moves,
        );
    }
}

#[cfg(test)]
mod test {
    use crate::baserules::board::PSBoard;
    use crate::baserules::board_rep::{BaseMove, PossibleMove};

    use crate::util::TryWithPanic;

    #[test]
    fn do_not_take_own_piece_to_castle() {
        let board =
            PSBoard::from_fen("r1b1kbnr/pppn1ppp/4p3/7Q/4Pq2/8/PPPP1PPP/RNB1K1NR w KQkq - 2 5");
        let mut moves = Vec::new();
        board.gen_king_moves("e1".transform(), &mut moves);
        let unacceptable_moves = [
            PossibleMove {
                the_move: BaseMove::from_uci("e1g1").unwrap(),
                pawn_promotion: None,
                rook: Some(BaseMove::from_uci("h1f1").unwrap()),
            },
            PossibleMove {
                the_move: BaseMove::from_uci("e1c1").unwrap(),
                pawn_promotion: None,
                rook: Some(BaseMove::from_uci("a1d1").unwrap()),
            },
        ];
        println!("{moves:?}");
        assert!(!moves
            .iter()
            .any(|a_move| unacceptable_moves.contains(a_move)));
    }
}
