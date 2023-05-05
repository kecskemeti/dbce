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

use crate::baserules::board::{
    black_can_castle, kingside_castle, queenside_castle, white_can_castle, Castling, PSBoard,
};
use crate::baserules::board_rep::{BaseMove, BoardPos, PossibleMove};
use crate::baserules::piece_color::PieceColor::*;
use crate::baserules::piece_kind::PieceKind;
use crate::baserules::piece_kind::PieceKind::*;
use enumset::EnumSet;
use std::cell::RefCell;
use std::cmp::{max, min};

impl PSBoard {
    /// does a dirty side switch to allow seeing castling issues
    fn switch_sides(&self) -> PSBoard {
        let mut other_side = self.clone();
        other_side.who_moves = if self.who_moves == White {
            Black
        } else {
            White
        };
        other_side
    }

    /// Pieces can move and take till they hit another piece, assuming it is not the same colour
    #[inline]
    fn piece_move_rule(&self, pos: BoardPos) -> bool {
        let target = self.get_loc(pos);
        if let Some(other_piece) = target.as_ref() {
            other_piece.color != self.who_moves
        } else {
            true
        }
    }

    /// Pawns can only move to empty spaces, cannot take in forward movement
    #[inline]
    fn pawn_move_rule(&self, pos: BoardPos) -> bool {
        self.get_loc(pos).is_none()
    }

    /// Pawns can take in diagonals, even with en passant
    #[inline]
    fn pawn_take_rule(&self, pos: BoardPos) -> bool {
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

    /// Determines if a particular move is allowed based on the piece movement rule and the coordinate validity
    /// if so it produces a new move
    #[inline]
    fn fil_map_core<I>(
        &self,
        row: i8,
        col: i8,
        loc: BoardPos,
        on_board_rule: &I,
    ) -> Option<PossibleMove>
    where
        I: Fn(&Self, BoardPos) -> bool,
    {
        if loc.0 & 7i8 == loc.0 && loc.1 & 7i8 == loc.1 &&
            // Move is on the board
            on_board_rule(self, loc)
        {
            // Move is allowed by the rule, we generate it
            Some(PossibleMove {
                the_move: BaseMove {
                    from: BoardPos(row, col),
                    to: BoardPos(loc.0, loc.1),
                },
                pawn_promotion: None,
                rook: None,
            })
        } else {
            None
        }
    }

    /// Generates a set of valid moves based on the coordinate adjustments passed in via the iterator
    /// Immediately deposits the moves in the output vector
    #[inline]
    fn gen_moves_from_dirs<'a, I: IntoIterator<Item = &'a BoardPos>, J>(
        &self,
        row: i8,
        col: i8,
        on_board_rule: &J,
        possible_moves: I,
        out: &mut Vec<PossibleMove>,
    ) where
        J: Fn(&Self, BoardPos) -> bool,
    {
        out.extend(
            possible_moves
                .into_iter()
                .map(|m| BoardPos(m.0 + row, m.1 + col))
                .filter_map(|loc| self.fil_map_core(row, col, loc, on_board_rule)),
        );
    }

    /// This does the same as `gen_moves_from_dirs`, but stops at the first occasion
    /// when moves are no longer possible
    #[inline]
    fn gen_moves_from_dirs_with_stop<'a, I, J>(
        &self,
        row: i8,
        col: i8,
        on_board_rule: &J,
        possible_moves: I,
        out: &mut Vec<PossibleMove>,
    ) where
        I: IntoIterator<Item = &'a BoardPos>,
        J: Fn(&Self, BoardPos) -> bool,
    {
        out.extend(
            possible_moves
                .into_iter()
                .map(|m| BoardPos(m.0 + row, m.1 + col))
                .map_while(|loc| self.fil_map_core(row, col, loc, on_board_rule)),
        );
    }

    /// This generates moves based on directional vectors (useful for rooks, bishops and queens)
    fn gen_moves_from_vecs<'a, I: IntoIterator<Item = &'a BoardPos>>(
        &self,
        row: i8,
        col: i8,
        vecs: I,
        out: &mut Vec<PossibleMove>,
    ) {
        for BoardPos(x, y) in vecs {
            static DIRECTIONAL_MOVES: [[BoardPos; 7]; 9] = [
                // this array is laid out so it is easy to map into it with the below formula using just the input coords
                [
                    BoardPos(-1, -1),
                    BoardPos(-2, -2),
                    BoardPos(-3, -3),
                    BoardPos(-4, -4),
                    BoardPos(-5, -5),
                    BoardPos(-6, -6),
                    BoardPos(-7, -7),
                ],
                [
                    BoardPos(-1, 0),
                    BoardPos(-2, 0),
                    BoardPos(-3, 0),
                    BoardPos(-4, 0),
                    BoardPos(-5, 0),
                    BoardPos(-6, 0),
                    BoardPos(-7, 0),
                ],
                [
                    BoardPos(-1, 1),
                    BoardPos(-2, 2),
                    BoardPos(-3, 3),
                    BoardPos(-4, 4),
                    BoardPos(-5, 5),
                    BoardPos(-6, 6),
                    BoardPos(-7, 7),
                ],
                [
                    BoardPos(0, -1),
                    BoardPos(0, -2),
                    BoardPos(0, -3),
                    BoardPos(0, -4),
                    BoardPos(0, -5),
                    BoardPos(0, -6),
                    BoardPos(0, -7),
                ],
                [
                    // filler to make the x-y mapping easier
                    BoardPos(0, 0),
                    BoardPos(0, 0),
                    BoardPos(0, 0),
                    BoardPos(0, 0),
                    BoardPos(0, 0),
                    BoardPos(0, 0),
                    BoardPos(0, 0),
                ],
                [
                    BoardPos(0, 1),
                    BoardPos(0, 2),
                    BoardPos(0, 3),
                    BoardPos(0, 4),
                    BoardPos(0, 5),
                    BoardPos(0, 6),
                    BoardPos(0, 7),
                ],
                [
                    BoardPos(1, -1),
                    BoardPos(2, -2),
                    BoardPos(3, -3),
                    BoardPos(4, -4),
                    BoardPos(5, -5),
                    BoardPos(6, -6),
                    BoardPos(7, -7),
                ],
                [
                    BoardPos(1, 0),
                    BoardPos(2, 0),
                    BoardPos(3, 0),
                    BoardPos(4, 0),
                    BoardPos(5, 0),
                    BoardPos(6, 0),
                    BoardPos(7, 0),
                ],
                [
                    BoardPos(1, 1),
                    BoardPos(2, 2),
                    BoardPos(3, 3),
                    BoardPos(4, 4),
                    BoardPos(5, 5),
                    BoardPos(6, 6),
                    BoardPos(7, 7),
                ],
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

    /// This figures out all the possible moves on the particular board
    ///
    /// # Example
    ///
    /// ```
    /// use dbce::baserules::board::PSBoard;
    /// use dbce::baserules::board_rep::BoardPos;
    /// let board = PSBoard::default();
    /// let mut moves = Vec::new();
    /// board.gen_potential_moves(true, &mut moves);
    /// let van_geet_opening_found = moves.iter().any(|m| m.the_move.from==BoardPos(0,1)&&m.the_move.to==BoardPos(2,2));
    /// assert!(van_geet_opening_found);
    /// ```
    pub fn gen_potential_moves(&self, castling_allowed: bool, the_moves: &mut Vec<PossibleMove>) {
        let king_move_call = if castling_allowed && !self.castling.is_empty() {
            PSBoard::gen_king_moves_with_castling
        } else {
            PSBoard::gen_king_moves
        };
        self.board
            .into_iter()
            .enumerate()
            .filter_map(|(idx, ps)| {
                if let Some(curr) = ps {
                    if curr.color == self.who_moves {
                        let ri = (idx >> 3) as i8;
                        let ci = (idx & 0b111) as i8;
                        return Some((
                            ri,
                            ci,
                            match curr.kind {
                                King => king_move_call,
                                Pawn => PSBoard::gen_pawn_moves,
                                Knight => PSBoard::gen_knight_moves,
                                Rook | Bishop | Queen => PSBoard::gen_vec_moves,
                            },
                        ));
                    }
                }
                None
            })
            .for_each(|(ri, ci, call)| call(self, ri, ci, the_moves));
    }

    /// Normal king moves
    fn gen_king_moves(&self, row: i8, col: i8, the_moves: &mut Vec<PossibleMove>) {
        self.gen_moves_from_dirs(
            row,
            col,
            &PSBoard::piece_move_rule,
            King.vec_moves(),
            the_moves,
        );
    }

    /// King moves when we are castling
    fn gen_king_moves_with_castling(&self, row: i8, col: i8, the_moves: &mut Vec<PossibleMove>) {
        self.gen_king_moves(row, col, the_moves);
        // Castling:
        if row == self.who_moves.piece_row() && col == 4 {
            // the king is in its original location we need a more in depth check on castling
            if (self.who_moves == White && !self.castling.is_disjoint(white_can_castle()))
                || (self.who_moves == Black && !self.castling.is_disjoint(black_can_castle()))
            {
                // there are castling opportunities
                static CASTLING_SIDE: [EnumSet<Castling>; 2] =
                    [queenside_castle(), kingside_castle()];
                static CASTLING_RANGES: [(i8, i8); 2] = [(1, 3), (5, 6)];
                static CASTLING_MOVES: [[i8; 3]; 2] = [[2, 0, 3], [6, 7, 5]];
                'outer: for (idx, side) in CASTLING_SIDE.iter().enumerate() {
                    if !self.castling.is_disjoint(*side) {
                        let mut castling_range_free = true;
                        for lc in CASTLING_RANGES[idx].0..CASTLING_RANGES[idx].1 {
                            castling_range_free &= self.get_loc(BoardPos(row, lc)).is_none();
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
                                    otherside.gen_potential_moves(false, &mut other_side_moves);
                                }
                                other_side_moves
                                    .iter()
                                    .filter(|m| {
                                        m.the_move.to.0 == self.who_moves.piece_row()
                                            && m.the_move.to.1 <= maxcol
                                            && m.the_move.to.1 >= mincol
                                    })
                                    .count()
                            };
                            if count == 0 {
                                // would not cross check
                                the_moves.push(PossibleMove {
                                    the_move: BaseMove {
                                        from: BoardPos(row, col),
                                        to: BoardPos(row, CASTLING_MOVES[idx][0]),
                                    },
                                    pawn_promotion: None,
                                    rook: Some(BaseMove {
                                        from: BoardPos(row, CASTLING_MOVES[idx][1]),
                                        to: BoardPos(row, CASTLING_MOVES[idx][2]),
                                    }),
                                });
                            }
                        }
                    }
                }
            }
        }
    }

    /// Pawn moves, takes and promotions
    fn gen_pawn_moves(&self, row: i8, col: i8, the_moves: &mut Vec<PossibleMove>) {
        // normal pawn move
        let prelen = the_moves.len();
        let pawn_move_now = if row == 1 || row == 6 {
            // two step pawn move at the beginning
            self.who_moves.pawn_double_step()
        } else {
            self.who_moves.pawn_single_step()
        };
        self.gen_moves_from_dirs_with_stop(
            row,
            col,
            &PSBoard::pawn_move_rule,
            pawn_move_now,
            the_moves,
        );

        // Pawn takes
        self.gen_moves_from_dirs(
            row,
            col,
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
    fn gen_knight_moves(&self, row: i8, col: i8, the_moves: &mut Vec<PossibleMove>) {
        self.gen_moves_from_dirs(
            row,
            col,
            &PSBoard::piece_move_rule,
            Knight.vec_moves(),
            the_moves,
        );
    }

    /// All other pieces have simple directional movement, so we just use their directions to generate their possible moves
    fn gen_vec_moves(&self, row: i8, col: i8, the_moves: &mut Vec<PossibleMove>) {
        self.gen_moves_from_vecs(
            row,
            col,
            self.get_loc(BoardPos(row, col)).unwrap().kind.vec_moves(),
            the_moves,
        );
    }
}

#[cfg(test)]
mod test {
    use crate::baserules::board::PSBoard;
    use crate::baserules::board_rep::PossibleMove;
    use crate::engine::Engine;
    use std::time::Duration;

    /// Tests for this game: https://lichess.org/NPchEbrvI0qD
    #[test]
    fn failed_game_1() {
        let board = PSBoard::from_fen("5rk1/2q2p1p/5Q2/3p4/1P2p1bP/P3P3/5PP1/R1r1K1NR w KQ - 1 26");
        let (engine, mut gamestate) =
            Engine::from_fen("5rk1/2q2p1p/5Q2/3p4/1P2p1bP/P3P3/2r2PP1/R3K1NR b KQ - 0 25");

        gamestate.make_a_human_move("Rc1+").unwrap();
        assert_eq!(format!("{board}"), format!("{}", gamestate.psboard()));

        let move_to_do = engine.best_move_for(&mut gamestate, &Duration::from_millis(1));
        assert_eq!(
            PossibleMove::simple_from_uci("a1c1").unwrap(),
            move_to_do.0.unwrap()
        );
    }

    /// Tests for this game: https://lichess.org/DbFqFBaYGgr6
    #[test]
    fn failed_game_2() {
        let board = PSBoard::from_fen("rnbk3r/1p1p3p/5Q1n/2N2P2/p7/8/PPP2KPP/R1B2B1R b - - 0 14");
        let (engine, mut gamestate) =
            Engine::from_fen("rnbk3r/1p1p3p/3Q1p1n/2N2P2/p7/8/PPP2KPP/R1B2B1R w - - 4 14");

        gamestate.make_a_human_move("Qxf6+").unwrap();
        assert_eq!(format!("{board}"), format!("{}", gamestate.psboard()));

        let move_to_do = engine.best_move_for(&mut gamestate, &Duration::from_millis(1));
        let acceptable_moves = [
            PossibleMove::simple_from_uci("d8c7").unwrap(),
            PossibleMove::simple_from_uci("d8e8").unwrap(),
        ];
        let the_move = move_to_do.0.unwrap();
        assert!(acceptable_moves
            .iter()
            .any(|acceptable| acceptable.eq(&the_move)));
    }

    /// Tests for this game: https://lichess.org/9KuuHpmFX74q
    /// Ideally, this test should not have such a long deadline that we have now
    #[test]
    fn failed_game_3() {
        let board = PSBoard::from_fen(
            "1rbq1knr/1npp2Q1/p4P1p/1p1P4/1P1B2p1/N2B4/P1P2PPP/1R3RK1 b - - 1 23",
        );
        let (engine, mut gamestate) =
            Engine::from_fen("1rbq1knr/1npp4/p4PQp/1p1P4/1P1B2p1/N2B4/P1P2PPP/1R3RK1 w - - 0 23");

        gamestate.make_a_human_move("Qg7+").unwrap();
        assert_eq!(format!("{board}"), format!("{}", gamestate.psboard()));

        let move_to_do = engine.best_move_for(&mut gamestate, &Duration::from_millis(1));
        assert_eq!(
            PossibleMove::simple_from_uci("f8e8").unwrap(),
            move_to_do.0.unwrap()
        );
    }
}
