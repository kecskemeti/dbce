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
use crate::baserules::piece_kind::PieceKind;
use crate::baserules::piece_kind::PieceKind::*;
use crate::baserules::positions::{AbsoluteBoardPos, RelativeBoardPos};
use crate::util::TryWithPanic;
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
        // Castling:
        // the king is in its original location we need a more in depth check on castling
        if ab == board.who_moves.starting_king_pos() {
            // A particular castling direction is allowed:
            for current_castling in board
                .castling
                .iter()
                .filter(|a_castling| board.who_moves.all_castling().contains(*a_castling))
            {
                let castling_move: &PossibleMove = current_castling.into();
                let mincol = min(castling_move.the_move.from.1, castling_move.the_move.to.1);
                let maxcol = max(castling_move.the_move.from.1, castling_move.the_move.to.1);
                // castling range is free of chess pieces
                if (mincol..=maxcol).all(|in_between_col| {
                    board[(castling_move.the_move.from.0, in_between_col)].is_none()
                }) {
                    // Is it free of potential checks on our king?
                    // Let's see if we would cross a check
                    let otherside = board.switch_sides();
                    let crosses_check = {
                        let mut other_side_moves = Vec::new();
                        {
                            otherside.gen_potential_moves(&mut other_side_moves);
                        }
                        other_side_moves.iter().any(|m| {
                            m.the_move.to.0 == castling_move.the_move.from.0
                                && m.the_move.to.1 <= maxcol
                                && m.the_move.to.1 >= mincol
                        })
                    };
                    if !crosses_check {
                        // would not cross check, the move is ok to emit
                        the_moves.push(*castling_move);
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
    pub(crate) fn switch_sides(&self) -> PSBoard {
        PSBoard {
            who_moves: self.who_moves.invert(),
            king_move_gen: &CASTLE_FORBIDDEN,
            ..*self
        }
    }

    /// Pieces can move and take till they hit another piece, assuming it is not the same colour
    #[inline]
    pub(crate) fn piece_move_rule(&self, pos: AbsoluteBoardPos) -> bool {
        self[pos]
            .as_ref()
            .map_or(true, |other_piece| other_piece.color != self.who_moves)
    }

    /// Pawns can only move to empty spaces, cannot take in forward movement
    #[inline]
    pub(crate) fn pawn_move_rule(&self, pos: AbsoluteBoardPos) -> bool {
        self[pos].is_none()
    }

    /// Pawns can take in diagonals, even with en passant
    #[inline]
    pub(crate) fn pawn_take_rule(&self, pos: AbsoluteBoardPos) -> bool {
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
            Some(BaseMove::from_two_pos(pos, loc).into())
        } else {
            None
        }
    }

    /// Generates a set of valid moves based on the coordinate adjustments passed in via the iterator
    /// Immediately deposits the moves in the output vector
    #[inline]
    pub(crate) fn gen_moves_from_dirs<'a, I: IntoIterator<Item = &'a RelativeBoardPos>, J>(
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
    pub(crate) fn gen_moves_from_dirs_with_stop<'a, I, J>(
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
    pub(crate) fn gen_moves_from_vecs<'a, I: IntoIterator<Item = &'a RelativeBoardPos>>(
        &self,
        position: AbsoluteBoardPos,
        vecs: I,
        out: &mut Vec<PossibleMove>,
    ) {
        // transform vec rel board
        for RelativeBoardPos(x, y) in vecs {
            // this array is laid out so it is easy to map into it with the below formula using just the input coords
            lazy_static! {
                static ref DIRECTIONAL_MOVES: [Vec<RelativeBoardPos>; 9] =
                [
                    directional_mapper(|i| (-i, -i)), // South West
                    directional_mapper(|i| (-i, 0)), // South
                    directional_mapper(|i| (-i, i)), // South East
                    directional_mapper(|i| (0, -i)), // West
                    directional_mapper(|_| (0,0)), // Centre
                    directional_mapper(|i| (0, i)), // East
                    directional_mapper(|i| (i, -i)), // North West
                    directional_mapper(|i| (i, 0)), // North
                    directional_mapper(|i| (i, i)), // North East
                ];
            }
            let curr_direction = &DIRECTIONAL_MOVES[(x + y + 2 * x + 4) as usize]; // the input coords directly map into the above array
            let allow_next = RefCell::new(true);
            self.gen_moves_from_dirs_with_stop(
                position,
                &move |board, checked_pos| {
                    if *allow_next.borrow() {
                        // Ensures we can take a piece but not go further or we need to stop a step before our own pieces
                        board[checked_pos].map_or(true, |piece_at_pos| {
                            let mut d = allow_next.borrow_mut();
                            *d = false;
                            !(piece_at_pos.color == board.who_moves)
                        })
                    } else {
                        false
                    }
                },
                curr_direction.iter(),
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
            .filter_map(|(idx, possible_piece)| {
                possible_piece.map_or(None, |piece| {
                    if piece.color == self.who_moves {
                        Some((idx, piece))
                    } else {
                        None
                    }
                })
            })
            .for_each(|(idx, piece)| piece.kind.gen_moves(self, idx.transform(), the_moves));
    }

    pub(crate) fn gen_king_moves(
        &self,
        position: AbsoluteBoardPos,
        the_moves: &mut Vec<PossibleMove>,
    ) {
        self.king_move_gen.gen_king_moves(self, position, the_moves);
    }

    /// Pawn moves, takes and promotions
    pub(crate) fn gen_pawn_moves(
        &self,
        position: AbsoluteBoardPos,
        the_moves: &mut Vec<PossibleMove>,
    ) {
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
}

fn directional_mapper<F>(mapper: F) -> Vec<RelativeBoardPos>
where
    F: Fn(i8) -> (i8, i8),
{
    RelativeBoardPos::transform_to_vec((1..=7).map(mapper))
}

#[cfg(test)]
mod test {
    use crate::baserules::board::PSBoard;
    use crate::baserules::board_rep::{BaseMove, PossibleMove};
    use crate::baserules::piece_kind::PieceKind;
    use std::collections::HashSet;

    use crate::util::TryWithPanic;

    #[tokio::test]
    async fn do_not_take_own_piece_to_castle() {
        let board =
            PSBoard::from_fen("r1b1kbnr/pppn1ppp/4p3/7Q/4Pq2/8/PPPP1PPP/RNB1K1NR w KQkq - 2 5")
                .await
                .unwrap();
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

    #[tokio::test]
    async fn all_rook_moves() {
        let board = PSBoard::from_fen("2b2rk1/p2p1ppp/8/P7/R2PPP2/2K5/7r/1R6 w - - 0 28")
            .await
            .unwrap();
        let mut moves = Vec::new();
        PieceKind::Rook.gen_moves(&board, "b1".transform(), &mut moves);
        let expected_moves: HashSet<String> = HashSet::from([
            "b1a1".into(),
            "b1c1".into(),
            "b1d1".into(),
            "b1e1".into(),
            "b1f1".into(),
            "b1g1".into(),
            "b1h1".into(),
            "b1b2".into(),
            "b1b3".into(),
            "b1b4".into(),
            "b1b5".into(),
            "b1b6".into(),
            "b1b7".into(),
            "b1b8".into(),
        ]);
        let found_moves: HashSet<_> = moves.iter().map(|amove| format!("{amove}")).collect();
        assert_eq!(expected_moves, found_moves);
    }
}
