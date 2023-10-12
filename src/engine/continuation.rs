use crate::baserules::board::PSBoard;
use crate::baserules::board_rep::PossibleMove;
use rand::{thread_rng, Rng};

use generational_arena::Arena;
use global_counter::primitive::fast::FlushingCounterU32;
use itertools::Itertools;
use rayon::prelude::*;
use std::{ops::Deref, sync::Arc};

#[derive(Clone)]
pub struct BoardContinuation {
    pub board: Arc<PSBoard>,
    /// The overall expected score of this board after considering the continuations
    pub adjusted_score: f32,
    /// If we have calculated a few positions ahead from this board, we store these positions here
    continuation: Arena<(PossibleMove, Self)>,
}

impl Default for BoardContinuation {
    /// Produces a board continuation with the starting board
    ///
    /// # Example use:
    /// ```
    /// use dbce::baserules::board::PSBoard;
    /// use dbce::baserules::rawboard::RawBoard;
    /// use dbce::engine::continuation::BoardContinuation;
    /// let starting_position = BoardContinuation::default();
    /// assert_eq!(starting_position.raw, PSBoard::default().raw);
    /// assert_eq!(0, starting_position.total_continuation_boards());
    /// ```
    fn default() -> Self {
        Self::new(PSBoard::default())
    }
}

/// Allows direct access to the PSBoard embedded inside BoardContinuations.
///
impl Deref for BoardContinuation {
    type Target = PSBoard;
    fn deref(&self) -> &Self::Target {
        &self.board
    }
}

impl<T> AsRef<T> for BoardContinuation
where
    T: ?Sized,
    <BoardContinuation as Deref>::Target: AsRef<T>,
{
    fn as_ref(&self) -> &T {
        self.deref().as_ref()
    }
}

impl BoardContinuation {
    pub fn new(board: PSBoard) -> Self {
        Self {
            board: Arc::new(board),
            adjusted_score: f32::NAN,
            continuation: Arena::new(),
        }
    }

    #[inline]
    pub fn make_cached_move(mut self, the_move: &PossibleMove) -> Self {
        if let Some(cont) = self.find_continuation_remove(the_move) {
            cont
        } else {
            Self::new(self.make_move_noncached(the_move))
        }
    }

    pub fn lookup_continuation_or_create<'a>(
        &'a mut self,
        amove: &PossibleMove,
        counter: &FlushingCounterU32,
    ) -> &'a mut Self {
        if self.continuation_exists(amove) {
            self.find_continuation_mut(amove).unwrap()
        } else {
            counter.inc();
            let psb = self.make_move_noncached(amove);
            self.insert_psboard(amove, psb);
            self.find_continuation_mut(amove).unwrap()
        }
    }

    pub fn continuation_exists(&self, the_move: &PossibleMove) -> bool {
        self.keys().any(|possible_move| possible_move == the_move)
    }

    pub fn is_leaf(&self) -> bool {
        self.continuation.is_empty()
    }

    pub fn search_leaves(&self) -> impl Iterator<Item = &BoardContinuation> {
        let direct_leaves: Box<dyn Iterator<Item = &BoardContinuation>> =
            Box::new(self.values().filter(|a| a.is_leaf()));
        let indirect_leaves: Box<dyn Iterator<Item = &BoardContinuation>> = Box::new(
            self.values()
                .filter(|a| !a.is_leaf())
                .flat_map(|b| b.search_leaves()),
        );
        direct_leaves.chain(indirect_leaves)
    }

    /*
    pub fn search_leaves_mut(&mut self) -> impl Iterator<Item = &mut BoardContinuation> {
        let direct_leaves: Box<dyn Iterator<Item = &mut BoardContinuation>> =
            Box::new(self.mut_values().filter(|a| a.is_leaf()));
        let indirect_leaves: Box<dyn Iterator<Item = &mut BoardContinuation>> = Box::new(
            self.mut_values()
                .filter(|a| !a.is_leaf())
                .flat_map(|b| b.search_leaves()),
        );
        direct_leaves.chain(indirect_leaves)
    }*/

    pub fn good_leaf_score(&self, score_drift: u8) -> Option<f32> {
        let score_comparator = self.who_moves.score_comparator();
        let mut score_options: Vec<_> = self.search_leaves().map(|board| board.score).collect();
        score_options.par_sort_by(move |t1, t2| score_comparator(t1, t2));

        score_options
            .get(
                score_options
                    .len()
                    .checked_sub(score_drift.into())
                    .unwrap_or_default(),
            )
            .cloned()
    }

    pub fn make_all_moves(
        &mut self,
        possible_moves: impl IntoIterator<Item = PossibleMove>,
        counter: &FlushingCounterU32,
    ) {
        possible_moves.into_iter().for_each(move |cur_move| {
            self.lookup_continuation_or_create(&cur_move, counter);
        });
    }

    /*
    pub fn one_good_continuation(&mut self, score_limit: f32, score_drift: u8) -> &mut Self {
        let who = self.who_moves;
        let choices = self.continuation.len().min(score_drift.into());
        self.search_leaves_mut()
            .filter(|board| who.is_better_score_or_equal(score_limit, board.score))
            .nth(thread_rng().gen_range(0..choices))
            .unwrap()
    }*/

    pub fn insert_psboard(&mut self, the_move: &PossibleMove, board: PSBoard) {
        self.continuation.insert((*the_move, Self::new(board)));
    }

    pub fn find_continuation_remove(&mut self, the_move: &PossibleMove) -> Option<Self> {
        let index_opt =
            self.continuation.iter().find_map(
                |(index, (amove, _))| {
                    if amove == the_move {
                        Some(index)
                    } else {
                        None
                    }
                },
            );
        index_opt.map(|index| self.continuation.remove(index).unwrap().1)
    }

    pub fn find_continuation(&self, the_move: &PossibleMove) -> Option<&Self> {
        self.iter().find_map(|(possible_move, continuation)| {
            if possible_move == the_move {
                Some(continuation)
            } else {
                None
            }
        })
    }

    pub fn find_continuation_mut(&mut self, the_move: &PossibleMove) -> Option<&mut Self> {
        self.continuation
            .iter_mut()
            .find_map(|(_, (possible_move, continuation))| {
                if possible_move == the_move {
                    Some(continuation)
                } else {
                    None
                }
            })
    }

    pub fn values(&self) -> impl Iterator<Item = &Self> {
        self.continuation
            .iter()
            .map(|(_, (_, continutation))| continutation)
    }

    pub fn mut_values(&mut self) -> impl Iterator<Item = &mut Self> {
        self.continuation
            .iter_mut()
            .map(|(_, (_, continutation))| continutation)
    }

    pub fn keys(&self) -> impl Iterator<Item = &PossibleMove> {
        self.continuation
            .iter()
            .map(|(_, (posssible_move, _))| posssible_move)
    }

    pub fn merge(&mut self, mut to_merge: Self) {
        to_merge
            .continuation
            .drain()
            .for_each(|(_, (amove, sub_continuation))| {
                if let Some(found_in_self) = self.find_continuation_mut(&amove) {
                    found_in_self.merge(sub_continuation);
                } else {
                    self.continuation.insert((amove, sub_continuation));
                }
            });
    }

    pub fn iter(&self) -> impl Iterator<Item = &(PossibleMove, Self)> {
        self.continuation.iter().map(|(_, tuple)| tuple)
    }

    pub fn similar_quality_moves<'a, F>(
        &'a self,
        best_board: &'a Self,
        score_query: F,
    ) -> impl Iterator<Item = &'a Self>
    where
        F: Fn(&Self) -> f32,
    {
        let bb_score = score_query(best_board);

        self.values().filter(move |other| {
            let other_score = score_query(other);
            (other_score.max(bb_score) - other_score.min(bb_score)) < 0.05
        })
    }

    pub fn select_similar_board<'a, F>(&'a self, best_board: &'a Self, score_query: F) -> &'a Self
    where
        F: Fn(&Self) -> f32,
    {
        let choices = self.similar_quality_moves(best_board, &score_query).count();
        self.similar_quality_moves(best_board, &score_query)
            .nth(thread_rng().gen_range(0..choices))
            .unwrap()
    }

    pub fn total_continuation_boards(&self) -> u32 {
        self.continuation.len() as u32
            + self
                .values()
                .map(|next_board| next_board.total_continuation_boards())
                .sum::<u32>()
    }

    #[allow(dead_code)]
    pub fn visualise_explored_moves(&self) -> String {
        self.prefixed_visualise_explored_moves("")
    }

    #[allow(dead_code)]
    pub fn prefixed_visualise_explored_moves(&self, prefix: &str) -> String {
        self.internal_visualise(prefix, 0)
    }

    fn internal_visualise(&self, prefix: &str, depth: usize) -> String {
        let next_depth = depth + 1;
        self.iter()
            .map(|(a_move, its_board)| {
                format!(
                    "{prefix}{:depth$}{a_move} ({}/{}) - {} \n{}",
                    "",
                    its_board.score,
                    its_board.adjusted_score,
                    its_board.to_fen(),
                    its_board.internal_visualise(prefix, next_depth)
                )
            })
            .join("")
    }

    pub fn score(&self) -> f32 {
        if self.adjusted_score.is_nan() {
            self.score
        } else {
            self.adjusted_score
        }
    }
}

#[cfg(test)]
mod test {
    use crate::baserules::board::PSBoard;
    use crate::baserules::board_rep::PossibleMove;
    use crate::engine::continuation::BoardContinuation;
    use generational_arena::Arena;
    use std::sync::Arc;

    fn create_simple_cont() -> BoardContinuation {
        let mut first = BoardContinuation {
            board: Arc::new(PSBoard::default()),
            adjusted_score: f32::NAN,
            continuation: Arena::new(),
        };
        let e2e4 = PossibleMove::simple_from_uci("e2e4").unwrap();
        first.insert_psboard(&e2e4, PSBoard::default().make_move_noncached(&e2e4));
        first
    }

    #[test]
    fn merge_two_simple() {
        let mut acont = create_simple_cont();
        let mut bcont = create_simple_cont();
        let first_move = *bcont.keys().next().unwrap();
        let e7e5 = PossibleMove::simple_from_uci("e7e5").unwrap();
        let new_board = bcont.make_move_noncached(&e7e5);
        let inner_cont = bcont.find_continuation_mut(&first_move).unwrap();
        inner_cont.insert_psboard(&e7e5, new_board);
        let btotal = bcont.total_continuation_boards();
        acont.merge(bcont);
        assert_eq!(acont.total_continuation_boards(), btotal);
    }
}
