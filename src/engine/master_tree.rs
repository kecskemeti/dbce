use std::vec;

use rand::{thread_rng, Rng};
use rayon::prelude::*;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::baserules::{board::PSBoard, board_rep::PossibleMove};

pub type MasterMoveResult = Result<MasterTree, (String, MasterTree)>;

/// Parent -> (child a) + (child b)
pub struct SubTree {
    pub board: PSBoard,
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
        self.leaves.extend(
            &mut completed_subtree
                .continuation
                .iter()
                .cloned()
                .map(|pmove| (completed_subtree.board, pmove)),
        );
        self.all_sub_trees.insert(move_index, completed_subtree);
    }

    pub fn good_leaf_score(&self, score_drift: u8) -> Option<f32> {
        let score_comparator = self.root.who_moves.score_comparator();
        let mut score_options: Vec<_> = self
            .leaves
            .iter()
            .map(|(board, _)| board.raw_score())
            .collect();
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

    fn remove_subtree(&mut self, the_move: PossibleMove, board: PSBoard) {
        let key = (board, the_move);
        if let Some(moves_not_taken) = self.all_sub_trees.remove(&key) {
            self.leaves.remove(&key);
            for mv in moves_not_taken.continuation {
                self.remove_subtree(mv, moves_not_taken.board);
            }
        }
    }

    /// method to make a move a(discard(master_tree))-> master_tree
    ///

    pub fn make_move(&mut self, the_move: PossibleMove) -> Result<(), String> {
        let mut moves_not_taken = Vec::new();
        self.root.gen_potential_moves(&mut moves_not_taken);
        if let Some((idx, _)) = moves_not_taken
            .iter()
            .enumerate()
            .find(|(idx, mv)| mv == &&the_move)
        {
            moves_not_taken.remove(idx);
            for mv in moves_not_taken {
                self.remove_subtree(mv, self.root);
            }
            let taken_key = (self.root, the_move);
            if let Some(chosen_subtree) = self.all_sub_trees.remove(&taken_key) {
                self.leaves.remove(&taken_key);
                self.root = chosen_subtree.board;
                Ok(())
            } else {
                Err(format!("no subtree available with this move {the_move}"))
            }
        } else {
            Err(format!(
                "unable to generate subtree with this move {the_move}"
            ))
        }

        // we have moves not taken
        // remove moves not taken classical loop
        // if leaf on move not taken; remove

        /*if let Some(new_root) = self.all_sub_trees.remove(&(self.root, the_move)) {
            let root = new_root.board;
            let mut all_sub_trees = FxHashMap::default();
            let mut leaves = FxHashSet::default();

            //new_root.continuation.into_iter().map(|mv|self.all_sub_trees.remove(&(root,mv)))
            for mv in new_root.continuation {
                let key = (root, mv);
                if let Some(child) = self.all_sub_trees.remove(&key) {
                    all_sub_trees.insert(key, child);
                } else {
                    // leaf
                    leaves.insert(key);
                }
            }

            Ok(MasterTree {
                all_sub_trees,
                leaves,
                root,
            })
        } else {
            Err((
                format!("no subtree available with this move {the_move}"),
                self,
            ))
        }*/
    }

    pub fn next_continuation(&mut self, score_limit: f32, score_drift: u8) -> SubTree {
        let who = self.root.who_moves;
        let choices = self.leaves.len().min(score_drift.into());
        let chosen = *self
            .leaves
            .iter()
            .filter(|(board, _)| who.is_better_score_or_equal(score_limit, board.raw_score()))
            .nth(thread_rng().gen_range(0..choices))
            .unwrap();
        self.leaves.remove(&chosen);
        let a = self.all_sub_trees.remove(&chosen);
        a.unwrap_or_else(|| {
            panic!(
                "Missing entry in all sub trees board: {} move: {}",
                chosen.0, chosen.1
            )
        })
    }
}
