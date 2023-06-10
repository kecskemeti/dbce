/*
 *  ========================================================================
 *  DBCE chess bot, core engine
 *  ========================================================================
 *
 *  This file is part of DBCE.
 *
 *  DBCE is free software: you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License as published
 *  by the Free Software Foundation, either version 3 of the License, or (at
 *  your option) any later version.
 *
 *  DBCE is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with DBCE.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  (C) Copyright 2022-3, Gabor Kecskemeti
 */
pub mod continuation;
mod enginethread;
pub mod gamestate;

use crate::baserules::board::PSBoard;
use crate::baserules::board_rep::PossibleMove;
use crate::baserules::piece_color::PieceColor;
use crate::baserules::piece_color::PieceColor::*;
use crate::util::DurationAverage;
use std::cmp::Ordering;
use std::ptr;
use std::sync::atomic::AtomicU8;
use std::sync::{Arc, Mutex};

use crate::baserules::rawboard::is_mate;
use crate::engine::continuation::BoardContinuation;
use crate::engine::enginethread::EngineThread;
use crate::engine::gamestate::GameState;
use global_counter::primitive::fast::FlushingCounterU32;
use std::thread;
use std::time::Duration;

#[derive(Clone)]
pub struct Engine {
    scoring_timings: Arc<Mutex<DurationAverage>>,
}

pub struct ExplorationInput<'a> {
    moves: Vec<PossibleMove>,
    start_board: &'a mut BoardContinuation,
    single_move_deadline: Duration,
    curr_depth: u8,
    max_search: [f32; 4],
    counter: &'a FlushingCounterU32,
    maximum: &'a AtomicU8,
    thread_info: &'a mut EngineThread,
}

pub struct ExplorationOutput {
    max_search: [f32; 4],
}

impl Engine {
    pub fn new() -> (Engine, GameState) {
        Engine::with_board_gen(PSBoard::default)
    }

    pub fn from_fen(fen: &str) -> (Engine, GameState) {
        Engine::with_board_gen(|| PSBoard::from_fen(fen))
    }

    fn with_board_gen(initial_board_provider: impl Fn() -> PSBoard) -> (Engine, GameState) {
        let sample_board = initial_board_provider();
        let mut moves = Vec::new();
        sample_board.gen_potential_moves(&mut moves);
        let mv = Mutex::new(0);
        let scoring_timings = DurationAverage::new(50, {
            move || {
                let mut mv = mv.lock().unwrap();
                let dur = EngineThread::timed_move(&sample_board, &moves[*mv % moves.len()]).1;
                *mv += 1;
                dur
            }
        });
        (
            Engine {
                scoring_timings: Arc::new(Mutex::new(scoring_timings)),
            },
            GameState {
                worked_on_board: BoardContinuation::new(initial_board_provider()),
            },
        )
    }

    fn manage_counter<T, F>(to_count: F) -> (T, u32, u8)
    where
        F: FnOnce(&FlushingCounterU32, &AtomicU8) -> T,
    {
        let board_counter = FlushingCounterU32::new(0);
        let maximum_depth = AtomicU8::new(0);
        let ret = to_count(&board_counter, &maximum_depth);
        board_counter.flush();
        (
            ret,
            board_counter.get(),
            maximum_depth.load(std::sync::atomic::Ordering::Acquire),
        )
    }

    // board count send instead of lock
    pub fn best_move_for(
        &self,
        state: &mut GameState,
        deadline: &Duration,
    ) -> (Option<PossibleMove>, f32, u32, u8) {
        let ((best_move, score), board_count, maximum) =
            Engine::manage_counter(|counter, maximum| {
                self.best_move_for_internal(
                    &mut state.worked_on_board,
                    deadline,
                    Engine::parallel_exploration,
                    0,
                    counter,
                    maximum,
                    &mut EngineThread::from(self),
                )
            });
        // let timings = self.scoring_timings.lock().unwrap();
        // println!("Overall average now: {:?}", timings.calc_average());
        (best_move, score, board_count, maximum)
    }

    // Determines the best move on the depth asked for
    // If the decide flag is passed, we will have the move generated, otherwise we just use this method for scoring
    fn best_move_for_internal<F>(
        &self,
        start_board: &mut BoardContinuation,
        deadline: &Duration,
        exploration_method: F,
        curr_depth: u8,
        counter: &FlushingCounterU32,
        maximum: &AtomicU8,
        thread_info: &mut EngineThread,
    ) -> (Option<PossibleMove>, f32)
    where
        F: Fn(&Self, ExplorationInput) -> ExplorationOutput,
    {
        let mut ret = (None, start_board.score);
        let mate_multiplier = match start_board.who_moves {
            White => 1.0,
            Black => -1.0,
        };

        if !is_mate(start_board.board.score) {
            let mut moves = Vec::new();
            start_board.gen_potential_moves(&mut moves);
            let movecount = moves.len();
            //println!("Potential moves: {:?}", moves);
            let single_move_deadline = deadline
                .checked_div(movecount as u32)
                .expect("Could not generate a single move!!!");
            // let width = curr_depth as usize;
            // println!(
            //     "{:3?} {:width$}Single move deadline: {:?}",
            //     thread::current().id(),
            //     "",
            //     single_move_deadline
            // );

            self.exploration(
                moves,
                start_board,
                single_move_deadline,
                exploration_method,
                curr_depth,
                counter,
                maximum,
                thread_info,
            );

            let best_potential_board = start_board.values().max_by(|b1, b2| {
                (mate_multiplier * b1.adjusted_score)
                    .partial_cmp(&(mate_multiplier * b2.adjusted_score))
                    .unwrap_or(Ordering::Less)
            });
            if let Some(best_board) = best_potential_board {
                let selected_board = if best_board.adjusted_score.is_nan() {
                    start_board.select_similar_board(best_board, |b| b.score)
                } else {
                    start_board.select_similar_board(best_board, |b| b.adjusted_score)
                };
                ret = start_board
                    .iter()
                    .find_map(|(amove, aboard)| {
                        if ptr::eq(aboard, selected_board) {
                            Some((
                                Some(*amove),
                                if selected_board.adjusted_score.is_nan() {
                                    selected_board.score
                                } else {
                                    selected_board.adjusted_score
                                },
                            ))
                        } else {
                            None
                        }
                    })
                    .unwrap();
            }
        } else {
            start_board.adjusted_score = start_board.score;
        }
        ret
    }

    fn sequential_exploration(&self, mut a: ExplorationInput) -> ExplorationOutput {
        let who = a.start_board.who_moves;
        let width = a.curr_depth as usize;
        while let Some(curr_move) = a.moves.pop() {
            // println!("{:width$} Start Board: {}", "", a.start_board.board);
            // println!("{:width$} Exploring move: {curr_move}", "");
            let board_with_move =
                a.thread_info
                    .timing_remembering_move(a.start_board, &curr_move, a.counter);
            // println!(
            //     "{:width$} Resulting Board: {} SCORE:{}",
            //     "", board_with_move.board, board_with_move.score
            // );
            let average_scoring_duration = a.thread_info.0.calc_average() * 40;
            let curr_score = if !is_mate(board_with_move.score)
                && (average_scoring_duration < a.single_move_deadline)
            {
                let (_, best_score) = self.best_move_for_internal(
                    board_with_move,
                    &a.single_move_deadline,
                    Engine::sequential_exploration,
                    a.curr_depth + 1,
                    a.counter,
                    a.maximum,
                    a.thread_info,
                );
                best_score
            } else {
                let mut val_before = a.maximum.load(std::sync::atomic::Ordering::Acquire);
                loop {
                    let cur_max = a
                        .maximum
                        .fetch_max(a.curr_depth, std::sync::atomic::Ordering::Acquire);
                    if cur_max == a.curr_depth {
                        let res = a.maximum.compare_exchange(
                            val_before,
                            cur_max,
                            std::sync::atomic::Ordering::Acquire,
                            std::sync::atomic::Ordering::Acquire,
                        );

                        if let Err(err) = res {
                            val_before = err;
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }
                board_with_move.score
            };

            Engine::update_max_search(who, &mut a.max_search, curr_score);
        }
        ExplorationOutput {
            max_search: a.max_search,
        }
    }

    fn update_max_search(who: PieceColor, max_search: &mut [f32], curr_score: f32) {
        if who == White {
            max_search.sort_unstable_by(|a, b| a.partial_cmp(b).unwrap());
            for (idx, a_good_score) in max_search.iter().enumerate() {
                if *a_good_score < curr_score {
                    max_search[idx] = curr_score;
                    break;
                }
            }
        } else {
            max_search.sort_unstable_by(|a, b| b.partial_cmp(a).unwrap());
            for (idx, a_good_score) in max_search.iter().enumerate() {
                if *a_good_score > curr_score {
                    max_search[idx] = curr_score;
                    break;
                }
            }
        }
    }

    fn exploration<F>(
        &self,
        moves: Vec<PossibleMove>,
        start_board: &mut BoardContinuation,
        single_move_deadline: Duration,
        exploration_method: F,
        curr_depth: u8,
        counter: &FlushingCounterU32,
        maximum: &AtomicU8,
        thread_info: &mut EngineThread,
    ) where
        F: Fn(&Self, ExplorationInput) -> ExplorationOutput,
    {
        start_board.adjusted_score = 0.0;
        let who = start_board.who_moves;
        let max_search = [if who == White {
            f32::NEG_INFINITY
        } else {
            f32::INFINITY
        }; 4];

        let mut max_search = exploration_method(
            self,
            ExplorationInput {
                moves,
                start_board,
                single_move_deadline,
                curr_depth,
                max_search,
                counter,
                maximum,
                thread_info,
            },
        )
        .max_search;

        max_search.sort_unstable_by(if who == White {
            |a: &f32, b: &f32| a.partial_cmp(b).unwrap()
        } else {
            |a: &f32, b: &f32| b.partial_cmp(a).unwrap()
        });
        for idx in 0..max_search.len() {
            let use_source_idx = if (max_search[idx] - max_search[3]).abs() > 10.0 {
                3 // Does not consider bad situations where there is only a few good moves
            } else {
                idx
            };
            max_search[idx] = max_search[use_source_idx] * (idx * 2 + 1) as f32;
            // Weighted towards the best scores
        }
        start_board.adjusted_score = (start_board.score
            + max_search
                .iter()
                .filter(|a_score| a_score.is_finite())
                .sum::<f32>())
            / 17f32; // sum of all weights + 1 for the start_board's base score.
    }

    fn parallel_exploration(&self, mut a: ExplorationInput) -> ExplorationOutput {
        thread::scope(|s| {
            let who = a.start_board.who_moves;

            let mut joins = Vec::new();
            let single_thread_deadline = a.single_move_deadline * (a.moves.len() as u32);
            let width = a.curr_depth as usize;

            while let Some(curr_move) = a.moves.pop() {
                let join_a = s.spawn({
                    let mut board_clone = a.start_board.clone();
                    let engine_clone = self.clone();
                    let mut thread_info_clone = a.thread_info.clone();
                    move || {
                        // println!("{:3?} working on {curr_move}", thread::current().id());
                        // println!("{:width$} Start Board: {}", "", board_clone.board);
                        // println!("{:width$} Exploring move: {curr_move}", "");
                        let board_with_move = thread_info_clone.timing_remembering_move(
                            &mut board_clone,
                            &curr_move,
                            a.counter,
                        );
                        // println!(
                        //     "{:width$} Resulting Board: {} SCORE:{}",
                        //     "", board_with_move.board, board_with_move.score
                        // );
                        let (_, curr_score) = engine_clone.best_move_for_internal(
                            board_with_move,
                            &single_thread_deadline,
                            Engine::sequential_exploration,
                            a.curr_depth + 1,
                            a.counter,
                            a.maximum,
                            &mut thread_info_clone,
                        );
                        a.counter.flush();
                        (
                            curr_score,
                            curr_move,
                            thread_info_clone.0.calc_average(),
                            board_clone,
                        )
                    }
                });

                joins.push(join_a);
            }

            for join in joins {
                let (curr_score, curr_move, timing_average, board_clone) = join.join().unwrap();
                // println!("Timings: {timing_average:?} for {curr_move}");
                Engine::update_max_search(who, &mut a.max_search, curr_score);
                {
                    let mut global_timings = self.scoring_timings.lock().unwrap();
                    global_timings.add(timing_average);
                }

                let width = a.curr_depth as usize;
                println!(
                    "{:width$} Evaluated move: {curr_move}, score: {}, adjusted: {}",
                    "", a.start_board.score, a.start_board.adjusted_score
                );

                a.start_board.merge(board_clone);
            }
            ExplorationOutput {
                max_search: a.max_search,
            }
        })
    }
}

#[cfg(test)]
mod test {
    use std::time::Duration;

    use crate::engine::{EngineThread, GameState};
    use crate::human_facing::helper;
    use crate::{baserules::board_rep::PossibleMove, engine::Engine};

    /// Test for this game: https://lichess.org/dRlJX08zhn1L
    #[test]
    fn weird_eval_1() {
        let (engine, mut gamestate) =
            Engine::from_fen("r1b1kbnr/pppn1ppp/4p3/6q1/4P3/8/PPPP1PPP/RNBQK1NR w KQkq - 0 4");
        let moves = vec![PossibleMove::simple_from_uci("d1h5").unwrap()];
        let mut thread_info = EngineThread::from(&engine);
        let result = Engine::manage_counter(|counter, maximum| {
            engine.exploration(
                moves,
                &mut gamestate.worked_on_board,
                Duration::from_millis(100),
                Engine::parallel_exploration,
                0,
                counter,
                maximum,
                &mut thread_info,
            )
        });
        println!("Depth: {}", result.2);
        println!("{}", gamestate.worked_on_board.adjusted_score);
        assert!(gamestate.worked_on_board.adjusted_score < -4.0);
    }

    /// Test for this game: https://lichess.org/dRlJX08zhn1L
    #[test]
    fn weird_eval_2() {
        let (engine, mut gamestate) =
            Engine::from_fen("r1b1kbnr/pppn1ppp/4p3/6qQ/4P3/8/PPPP1PPP/RNB1K1NR b KQkq - 1 4");
        let (_, (_, score)) =
            helper::calculate_move_for_console(&engine, &mut gamestate, &Duration::from_secs(2));
        assert!(score < -6.0);
    }

    /// Test for this game: https://lichess.org/dRlJX08zhn1L
    #[test]
    fn weird_eval_3() {
        let (engine, mut gamestate) =
            Engine::from_fen("r1b1kbnr/pppn1ppp/4p3/6qQ/4P3/8/PPPP1PPP/RNB1K1NR b KQkq - 1 4");
        let moves = vec![PossibleMove::simple_from_uci("g5d2").unwrap()];
        let mut thread_info = EngineThread::from(&engine);
        let result = Engine::manage_counter(|counter, maximum| {
            engine.exploration(
                moves,
                &mut gamestate.worked_on_board,
                Duration::from_millis(100),
                Engine::parallel_exploration,
                0,
                counter,
                maximum,
                &mut thread_info,
            )
        });
        println!("Depth: {}", result.2);
        println!("{}", gamestate.worked_on_board.adjusted_score);
        assert!(gamestate.worked_on_board.adjusted_score > 2.0);
    }

    /// Test for this game: https://lichess.org/ZnIAbaQXqHCF
    #[test]
    fn weird_eval_4() {
        let (engine, mut gamestate) =
            Engine::from_fen("r2qk2r/pp1nbppp/2p5/5b2/4p3/PQ6/1P1PPPPP/R1B1KBNR w KQkq - 4 11");
        let moves = vec![PossibleMove::simple_from_uci("b3f7").unwrap()];
        let mut thread_info = EngineThread::from(&engine);
        let result = Engine::manage_counter(|counter, maximum| {
            engine.exploration(
                moves,
                &mut gamestate.worked_on_board,
                Duration::from_secs(1),
                Engine::parallel_exploration,
                0,
                counter,
                maximum,
                &mut thread_info,
            )
        });
        println!("Depth: {}", result.2);
        println!("{}", gamestate.worked_on_board.adjusted_score);
        assert!(gamestate.worked_on_board.adjusted_score < -5.0);
    }

    /// Test for this game: https://lichess.org/ZnIAbaQXqHCF
    #[test]
    fn weird_eval_4_1() {
        let (engine, mut gamestate) =
            Engine::from_fen("r2qk2r/pp1nbppp/2p5/5b2/4p3/PQ6/1P1PPPPP/R1B1KBNR w KQkq - 4 11");
        let (_, (best, _)) =
            helper::calculate_move_for_console(&engine, &mut gamestate, &Duration::from_secs(2));
        assert!(!best
            .unwrap()
            .eq(&PossibleMove::simple_from_uci("b3f7").unwrap()));
    }

    impl GameState {
        fn make_a_move_pair(&mut self, engine_move: &str, opponent_move: &str) {
            self.make_a_human_move_or_panic(engine_move);
            self.make_a_human_move_or_panic(opponent_move);
        }
    }

    impl Engine {
        fn build_continuation_and_move(
            &self,
            gamestate: &mut GameState,
            deadline: &Duration,
            engine_move: &str,
            opponent_move: &str,
        ) {
            helper::calculate_move_for_console(self, gamestate, deadline);
            gamestate.make_a_move_pair(engine_move, opponent_move);
        }
    }

    /// Test for this game: https://lichess.org/5fa8V5PVXDEL
    #[test]
    fn weird_eval_5() {
        let (engine, mut gamestate) =
            Engine::from_fen("2b2rk1/p2p1ppp/8/P7/R2PPP2/8/1r1K2PP/5R2 w - - 0 26");
        let initial_duration = Duration::from_secs(20);
        engine.build_continuation_and_move(&mut gamestate, &initial_duration, "Kc3", "Rxg2");
        let normal_duration = Duration::from_secs(8);
        engine.build_continuation_and_move(&mut gamestate, &normal_duration, "Rb1", "Rxh2");
        engine.build_continuation_and_move(&mut gamestate, &normal_duration, "Rb8", "Re8");
        engine.build_continuation_and_move(&mut gamestate, &normal_duration, "Rc4", "d6");
        engine.build_continuation_and_move(&mut gamestate, &normal_duration, "Rcxc8", "Rh3+");
        let (_, (best, _)) =
            helper::calculate_move_for_console(&engine, &mut gamestate, &normal_duration);
        let acceptable_moves = [
            PossibleMove::simple_from_uci("c3c2").unwrap(),
            PossibleMove::simple_from_uci("c3d2").unwrap(),
            PossibleMove::simple_from_uci("c3b2").unwrap(),
            PossibleMove::simple_from_uci("c3c4").unwrap(),
            PossibleMove::simple_from_uci("c3b4").unwrap(),
        ];
        let the_move = best.unwrap();
        assert!(acceptable_moves
            .iter()
            .any(|acceptable| acceptable.eq(&the_move)));
    }

    /// Tests for this game: https://lichess.org/73Bl5rBonV45
    fn prep_failed_game_4() -> (Engine, GameState) {
        let (engine, mut gamestate) =
            Engine::from_fen("rn2kbnr/p1q1pNpp/1pp1P3/3p4/8/2N5/PPP1QPPP/R1B1KR2 b Qkq - 2 11");
        let normal_duration = Duration::from_secs(1);
        engine.build_continuation_and_move(&mut gamestate, &normal_duration, "d4", "Nxh8");
        engine.build_continuation_and_move(&mut gamestate, &normal_duration, "dxc3", "Qh5+");
        engine.build_continuation_and_move(&mut gamestate, &normal_duration, "Kd8", "Nf7+");
        (engine, gamestate)
    }

    /// Tests for this game: https://lichess.org/73Bl5rBonV45
    #[test]
    fn failed_game_4() {
        let (engine, mut gamestate) = prep_failed_game_4();
        let (_, move_to_do) =
            helper::calculate_move_for_console(&engine, &mut gamestate, &Duration::from_secs(1));
        let acceptable_moves = [
            PossibleMove::simple_from_uci("d8c8").unwrap(),
            PossibleMove::simple_from_uci("d8e8").unwrap(),
        ];
        let the_move = move_to_do.0.unwrap();
        //        visualise_explored_moves(gamestate.get_board());
        assert!(acceptable_moves
            .iter()
            .any(|acceptable| acceptable.eq(&the_move)));
    }

    /// Tests for this game: https://lichess.org/73Bl5rBonV45
    #[test]
    fn failed_game_4_subtest() {
        let (engine, mut gamestate) = prep_failed_game_4();
        helper::calculate_move_for_console(&engine, &mut gamestate, &Duration::from_secs(1));
        gamestate.make_a_human_move_or_panic("cxb2");
        let the_board = gamestate.continuation().clone();
        let mut moves = Vec::new();
        the_board.gen_potential_moves(&mut moves);
        println!("{moves:?}");
        assert!(moves.contains(&PossibleMove::simple_from_uci("f7d8").unwrap()));
    }

    #[test]
    fn retain_boards() {
        let (engine, mut gamestate) = Engine::from_fen("8/8/8/8/6PP/6Pk/7P/7K w - - 0 1");
        let short_deadline = Duration::from_millis(1);
        let (_, _, _, depth) = engine.best_move_for(&mut gamestate, &short_deadline);
        println!("Max Depth: {depth}");
        gamestate.worked_on_board.visualise_explored_moves();
        let a_selected_move = *gamestate.worked_on_board.keys().next().unwrap();
        let the_selected_board = gamestate
            .worked_on_board
            .find_continuation(&a_selected_move)
            .unwrap();
        let continuations_before = the_selected_board.total_continuation_boards();
        gamestate.make_a_generated_move(&a_selected_move);
        let (_, _, board_count, depth) = engine.best_move_for(&mut gamestate, &short_deadline);
        println!("Max Depth: {depth}");
        gamestate.worked_on_board.visualise_explored_moves();
        let continuations_after = gamestate.worked_on_board.total_continuation_boards();
        assert_eq!(continuations_after - continuations_before, board_count);
    }
}
