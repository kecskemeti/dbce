use crate::baserules::board::PSBoard;
use crate::baserules::board_rep::PossibleMove;
use crate::engine::continuation::BoardContinuation;
use crate::engine::Engine;
use crate::util::DurationAverage;
use global_counter::primitive::fast::FlushingCounterU32;
use std::time::Duration;
use tokio::time::Instant;

#[derive(Clone)]
pub struct EngineThread(pub(crate) DurationAverage);

impl EngineThread {
    pub fn timed_move(board: &PSBoard, amove: &PossibleMove) -> (PSBoard, Duration) {
        let pre = Instant::now();
        (board.make_move_noncached(amove), pre.elapsed())
    }

    pub fn timing_remembering_move<'a>(
        &mut self,
        board: &'a mut BoardContinuation,
        amove: &PossibleMove,
        counter: &FlushingCounterU32,
    ) -> &'a mut BoardContinuation {
        counter.inc();
        if board.continuation.contains_key(amove) {
            board.continuation.get_mut(amove).unwrap()
        } else {
            let (psb, dur) = EngineThread::timed_move(&board.board, amove);
            self.0.add(dur);
            board
                .continuation
                .insert(*amove, BoardContinuation::new(psb));
            board.continuation.get_mut(amove).unwrap()
        }
    }
}

impl From<&Engine> for EngineThread {
    fn from(engine: &Engine) -> Self {
        EngineThread(engine.scoring_timings.lock().unwrap().clone())
    }
}
