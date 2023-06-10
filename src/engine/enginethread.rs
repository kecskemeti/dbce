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
        if board.continuation_exists(amove) {
            board.find_continuation_mut(amove).unwrap()
        } else {
            counter.inc();
            let (psb, dur) = EngineThread::timed_move(board, amove);
            self.0.add(dur);
            board.insert_psboard(amove, psb);
            board.find_continuation_mut(amove).unwrap()
        }
    }
}

impl From<&Engine> for EngineThread {
    fn from(engine: &Engine) -> Self {
        EngineThread(engine.scoring_timings.lock().unwrap().clone())
    }
}
