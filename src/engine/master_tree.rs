use std::collections::HashMap;

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

impl SubTree {
    pub fn calculate_continuation(board: PSBoard) -> SubTree {
        let mut continuation = Vec::new();
        board.gen_potential_moves(&mut continuation);
        SubTree {
            continuation,
            adjusted_score: f32::NAN,
            board,
        }
    }
}

pub struct MasterTree {
    pub all_sub_trees: FxHashMap<(PSBoard, PossibleMove), SubTree>,
    pub leaves: FxHashSet<(PSBoard, PossibleMove)>,
    pub root: PSBoard,
}

impl MasterTree {
    pub fn new(root: PSBoard) -> Self {
        let mut continuation = Vec::new();
        root.gen_potential_moves(&mut continuation);
        MasterTree {
            all_sub_trees: HashMap::default(),
            leaves: continuation.iter().map(|o| (root, *o)).collect(),
            root,
        }
    }

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
            .find(|(_, mv)| mv == &&the_move)
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

#[cfg(test)]
mod tests {
    use crate::baserules::board::PSBoard;
    use crate::baserules::board_rep::PossibleMove;

    use super::{MasterTree, SubTree};

    fn prep_subtree() -> (SubTree, PossibleMove) {
        let b = PSBoard::default();
        let mut mvs = Vec::new();
        b.gen_potential_moves(&mut mvs);
        let first_move = mvs[0];
        (
            SubTree::calculate_continuation(b.make_move_noncached(&first_move)),
            first_move,
        )
    }

    #[test]
    fn save_results_in_leaves() {
        let mut master_tree = MasterTree::new(PSBoard::default());
        let (sub_tree, first_move) = prep_subtree();
        let continuation_copy = sub_tree.continuation.clone();
        let board_copy = sub_tree.board;
        master_tree.save(sub_tree, (master_tree.root, first_move));
        assert!(continuation_copy
            .iter()
            .all(|val| master_tree.leaves.contains(&(board_copy, *val))));
    }

    #[test]
    fn good_leaf_score_returns_best_score() {
        let mut master_tree = MasterTree::new(PSBoard::default());
        let (sub_tree, first_move) = prep_subtree();
        master_tree.save(sub_tree, (master_tree.root, first_move));
        assert_eq!(master_tree.good_leaf_score(1).unwrap(), 0f32);
    }

    #[test]
    fn make_move() {
        let mut master_tree = MasterTree::new(PSBoard::default());
        let (sub_tree, first_move) = prep_subtree();
        let board_copy = sub_tree.board;
        master_tree.save(sub_tree, (master_tree.root, first_move));
        assert!(master_tree.make_move(first_move).is_ok());
        assert_eq!(master_tree.root, board_copy);
    }

    #[test]
    fn get_subtree_to_work_on() {
        let mut master_tree = MasterTree::new(PSBoard::default());
        let (sub_tree, first_move) = prep_subtree();
        let board_copy = sub_tree.board;
        master_tree.save(sub_tree, (master_tree.root, first_move));
        let return_tree = master_tree.next_continuation(0f32, 1);
        assert_eq!(board_copy, return_tree.board);
        assert_eq!(master_tree.all_sub_trees.len(), 0);
    }
}
