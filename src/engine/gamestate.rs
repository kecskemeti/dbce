use crate::baserules::board_rep::PossibleMove;
use crate::engine::continuation::BoardContinuation;
use crate::human_facing::moves::{make_a_human_move, make_an_uci_move};
use crate::util::{EmptyResult, IntResult};
use std::mem;

pub struct GameState {
    pub(crate) worked_on_board: BoardContinuation,
}

impl GameState {
    #[inline]
    pub fn get_board(&self) -> &BoardContinuation {
        &self.worked_on_board
    }

    #[inline]
    pub fn make_an_uci_move(&mut self, themove: &str) -> EmptyResult {
        self.replace_board_after_move(|old_board| make_an_uci_move(old_board, themove))
    }

    #[inline]
    pub fn make_a_human_move(&mut self, themove: &str) -> EmptyResult {
        self.replace_board_after_move(|old_board| {
            make_a_human_move(old_board, themove).ok_or("Conversion error".into())
        })
    }

    #[inline]
    pub fn make_a_human_move_or_panic(&mut self, themove: &str) {
        self.make_a_human_move(themove).unwrap()
    }

    #[inline]
    pub fn make_a_generated_move(&mut self, themove: &PossibleMove) {
        self.replace_board_after_move(|old_board| Ok(old_board.make_cached_move(themove)))
            .unwrap();
    }

    fn replace_board_after_move<F>(&mut self, replacement_producer_for: F) -> EmptyResult
    where
        F: FnOnce(BoardContinuation) -> IntResult<BoardContinuation>,
    {
        let old_board = mem::take(&mut self.worked_on_board);
        self.worked_on_board = replacement_producer_for(old_board)?;
        Ok(())
    }
}