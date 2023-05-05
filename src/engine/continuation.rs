use crate::baserules::board::PSBoard;
use crate::baserules::board_rep::PossibleMove;
use rand::{thread_rng, Rng};
use std::collections::BTreeMap;
use std::sync::Arc;

#[derive(Clone)]
pub struct BoardContinuation {
    pub board: Arc<PSBoard>,
    /// The overall expected score of this board after considering the continuations
    pub adjusted_score: f32,
    /// If we have calculated a few positions ahead from this board, we store these positions here
    pub continuation: BTreeMap<PossibleMove, BoardContinuation>,
}

impl Default for BoardContinuation {
    fn default() -> Self {
        BoardContinuation::new(PSBoard::default())
    }
}

impl BoardContinuation {
    pub fn new(of_board: PSBoard) -> BoardContinuation {
        BoardContinuation {
            board: Arc::new(of_board),
            adjusted_score: f32::NAN,
            continuation: BTreeMap::new(),
        }
    }

    #[inline]
    pub fn make_cached_move(mut self, the_move: &PossibleMove) -> BoardContinuation {
        if let Some(cont) = self.continuation.remove(the_move) {
            cont
        } else {
            BoardContinuation::new(self.board.make_move_noncached(the_move))
        }
    }

    pub fn similar_quality_moves<'a, F>(
        &'a self,
        best_board: &'a BoardContinuation,
        score_query: F,
    ) -> impl Iterator<Item = &'a BoardContinuation>
    where
        F: Fn(&BoardContinuation) -> f32,
    {
        let bb_score = score_query(best_board);
        self.continuation.values().filter(move |other| {
            let other_score = score_query(other);
            (other_score.max(bb_score) - other_score.min(bb_score)) < 0.05
        })
    }

    pub fn select_similar_board<'a, F>(
        &'a self,
        best_board: &'a BoardContinuation,
        score_query: F,
    ) -> &'a BoardContinuation
    where
        F: Fn(&BoardContinuation) -> f32,
    {
        let choices = self.similar_quality_moves(best_board, &score_query).count();
        self.similar_quality_moves(best_board, &score_query)
            .nth(thread_rng().gen_range(0..choices))
            .unwrap()
    }
}
