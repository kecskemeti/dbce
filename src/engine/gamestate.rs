use crate::baserules::board::PSBoard;
use crate::baserules::board_rep::PossibleMove;
use crate::engine::continuation::BoardContinuation;
use crate::human_facing::moves::{make_a_human_move, make_an_uci_move, BoardParseResult};
use crate::util::EmptyResult;
use std::mem;

pub struct GameState {
    pub(crate) worked_on_board: BoardContinuation,
}

impl GameState {
    #[inline]
    pub fn continuation(&self) -> &BoardContinuation {
        &self.worked_on_board
    }

    #[inline]
    pub fn psboard(&self) -> &PSBoard {
        &self.worked_on_board
    }

    #[inline]
    pub fn make_an_uci_move(&mut self, themove: &str) -> EmptyResult {
        self.replace_board_after_move(|old_board| make_an_uci_move(old_board, themove))
    }

    #[inline]
    pub fn make_a_human_move(&mut self, themove: &str) -> EmptyResult {
        self.replace_board_after_move(|old_board| make_a_human_move(old_board, themove))
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

    pub fn new(board: PSBoard) -> GameState {
        GameState {
            worked_on_board: BoardContinuation::new(board),
        }
    }

    fn replace_board_after_move<F>(&mut self, replacement_producer_for: F) -> EmptyResult
    where
        F: FnOnce(BoardContinuation) -> BoardParseResult,
    {
        let old_board = mem::take(&mut self.worked_on_board);
        let potential_result_board = replacement_producer_for(old_board);
        match potential_result_board {
            Ok(result_board) => {
                self.worked_on_board = result_board;
                Ok(())
            }
            Err((error_message, old_board)) => {
                self.worked_on_board = old_board;
                Err(error_message.into())
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::baserules::board::PSBoard;
    use crate::engine::gamestate::GameState;

    #[test]
    pub fn move_error_should_return_to_state_before() {
        let kasparov_immortal_fen = "b2r3r/k4p1p/p2q1np1/NppP4/3R1Q2/P4PPB/1PP4P/1K2R3 b - - 0 24";
        let mut kasparov_immortal =
            GameState::new(PSBoard::from_fen(kasparov_immortal_fen).unwrap());
        assert!(kasparov_immortal.make_a_human_move("Rxd5").is_err());
        assert_eq!(
            kasparov_immortal_fen,
            kasparov_immortal.worked_on_board.to_fen()
        );
    }
}
