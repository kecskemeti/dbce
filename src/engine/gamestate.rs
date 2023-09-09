use crate::baserules::board::PSBoard;
use crate::baserules::board_rep::PossibleMove;
use crate::engine::continuation::BoardContinuation;
use crate::human_facing::moves::{make_a_human_move, make_an_uci_move, BoardParseResult};
use crate::util::EmptyResult;

use std::mem;

#[async_trait::async_trait]
pub trait ReplacementGenerator {
    async fn produce_replacment(&self, board: BoardContinuation) -> BoardParseResult;
}

struct GeneratedMove {
    themove: PossibleMove,
}
#[async_trait::async_trait]
impl ReplacementGenerator for GeneratedMove {
    async fn produce_replacment(&self, old_board: BoardContinuation) -> BoardParseResult {
        Ok(old_board.make_cached_move(&self.themove).await)
    }
}

struct UciMove {
    themove: String,
}
#[async_trait::async_trait]
impl ReplacementGenerator for UciMove {
    async fn produce_replacment(&self, old_board: BoardContinuation) -> BoardParseResult {
        make_an_uci_move(old_board, &self.themove).await
    }
}

struct HumanMove {
    themove: String,
}
#[async_trait::async_trait]
impl ReplacementGenerator for HumanMove {
    async fn produce_replacment(&self, old_board: BoardContinuation) -> BoardParseResult {
        make_a_human_move(old_board, &self.themove).await
    }
}

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
    pub async fn make_an_uci_move(&mut self, the_move: &str) -> EmptyResult {
        self.replace_board_after_move(UciMove {
            themove: the_move.to_string(),
        })
        .await
    }

    #[inline]
    pub async fn make_a_human_move(&mut self, the_move: &str) -> EmptyResult {
        self.replace_board_after_move(HumanMove {
            themove: the_move.to_string(),
        })
        .await
    }

    #[inline]
    pub async fn make_a_human_move_or_panic(&mut self, themove: &str) {
        self.make_a_human_move(themove).await.unwrap()
    }

    #[inline]
    pub async fn make_a_generated_move(&mut self, the_move: &PossibleMove) {
        self.replace_board_after_move(GeneratedMove { themove: *the_move })
            .await
            .unwrap();
    }

    pub fn new(board: PSBoard) -> GameState {
        GameState {
            worked_on_board: BoardContinuation::new(board),
        }
    }

    async fn replace_board_after_move(
        &mut self,
        generator: impl ReplacementGenerator,
    ) -> EmptyResult {
        let old_board = mem::take(&mut self.worked_on_board);
        let potential_result_board = generator.produce_replacment(old_board).await;
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

    #[tokio::test]
    pub async fn move_error_should_return_to_state_before() {
        let kasparov_immortal_fen = "b2r3r/k4p1p/p2q1np1/NppP4/3R1Q2/P4PPB/1PP4P/1K2R3 b - - 0 24";
        let mut kasparov_immortal =
            GameState::new(PSBoard::from_fen(kasparov_immortal_fen).await.unwrap());
        assert!(kasparov_immortal.make_a_human_move("Rxd5").await.is_err());
        assert_eq!(
            kasparov_immortal_fen,
            kasparov_immortal.worked_on_board.to_fen()
        );
    }
}
