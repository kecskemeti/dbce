use std::sync::Arc;

use rand::thread_rng;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::baserules::{board::PSBoard, board_rep::PossibleMove};

/// Parent -> (child a) + (child b)
pub struct SubTree {
    pub board: Arc<PSBoard>,
    /// The overall expected score of this board after considering the continuations
    pub adjusted_score: f32,
    pub continuation: Vec<PossibleMove>,
}

pub struct MasterTree {
    pub all_sub_trees: FxHashMap<(PSBoard, PossibleMove), SubTree>,
    pub leaves: FxHashSet<(PSBoard, PossibleMove)>,
    pub root: PSBoard,
}

impl MasterTree {
    pub fn save(&mut self, completed_subtree: SubTree, move_index: (PSBoard, PossibleMove)) {
        self.leaves.remove(&move_index);
        self.leaves.append(
            &mut completed_subtree
                .continuation
                .iter()
                .cloned()
                .map(|pmove| (*completed_subtree.board, pmove))
                .collect::<Vec<_>>(),
        );
        self.all_sub_trees.insert(move_index, completed_subtree);
    }

    pub fn good_leaf_score(&self, score_drift: u8) -> Option<f32> {
        let score_comparator = self.root.who_moves.score_comparator();
        let mut score_options: Vec<_> = self.leaves.iter().map(|(board, _)| board.score).collect();
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

    /// method to make a move a(discard(master_tree))-> master_tree

    /// return a subtree not & - remove chosen from leaves and all subtress instead of get
    pub fn next_continuation(&mut self, score_limit: f32, score_drift: u8) -> SubTree {
        let who = self.root.who_moves;
        let choices = self.leaves.len().min(score_drift.into());
        let chosen = self
            .leaves
            .iter()
            .filter(|(board, _)| who.is_better_score_or_equal(score_limit, board.score))
            .nth(thread_rng().gen_range(0..choices))
            .unwrap();
        self.all_sub_trees.get(chosen).unwrap_or(|| {
            panic!(
                "Missing entry in all sub trees board: {} move: {}",
                chosen.0, chosen.1
            )
        })
    }
}
