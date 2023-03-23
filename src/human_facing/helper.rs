use crate::baserules::board::PSBCOUNT;
use crate::baserules::board_rep::PossibleMove;
use crate::engine::{Engine, GameState};
use std::time::Duration;
use tokio::time::Instant;

pub fn calculate_move_for_console(
    engine: &mut Engine,
    gamestate: &mut GameState,
    deadline: &Duration,
) -> (Duration, (Option<PossibleMove>, f32)) {
    let ins = Instant::now();
    unsafe {
        PSBCOUNT = 0;
    }
    let machine_eval = engine.best_move_for(gamestate, deadline);
    let machine_move = machine_eval.0.as_ref().unwrap();
    let taken_this_much_time = ins.elapsed();
    let taken_this_much_time_ms = ins.elapsed().as_millis();
    println!("Move took {} ms", taken_this_much_time_ms);
    unsafe {
        println!(
            "{} kNodes/sec",
            u128::from(PSBCOUNT) / 1.max(taken_this_much_time_ms)
        );
    }
    println!("Evaluation result: {machine_move:?}");
    (taken_this_much_time, machine_eval)
}
