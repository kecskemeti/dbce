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
use crate::baserules::board::{PSBoard, MATE};
use crate::baserules::board_rep::PossibleMove;
use crate::baserules::piece_color::PieceColor;
use crate::baserules::piece_color::PieceColor::*;
use crate::human_facing::moves::{make_a_human_move, make_an_uci_move};
use crate::util::{DurationAverage, VecCache};
use rand::{thread_rng, Rng};
use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::error::Error;
use std::ptr;
// use std::sync::mpsc::SyncSender;
use std::sync::mpsc::{self, Sender};

use std::thread;
use std::time::Duration;
use tokio::time::Instant;

#[derive(Clone)]
pub struct Engine {
    move_cache: VecCache<PossibleMove>,
    scoring_timings: DurationAverage,
}

pub struct GameState {
    worked_on_board: PSBoard,
}

impl GameState {
    pub fn get_board(&self) -> &PSBoard {
        &self.worked_on_board
    }

    pub fn make_an_uci_move(&mut self, themove: &str) -> Result<(), Box<dyn Error>> {
        self.worked_on_board = make_an_uci_move(&mut self.worked_on_board, themove)?;
        Ok(())
    }

    pub fn make_a_human_move(&mut self, themove: &str) -> Result<(), Box<dyn Error>> {
        self.worked_on_board =
            make_a_human_move(&mut self.worked_on_board, themove).ok_or("Conversion error")?;
        Ok(())
    }

    pub fn make_a_generated_move(&mut self, themove: &PossibleMove) {
        self.worked_on_board = self.worked_on_board.make_a_move(themove);
    }
}

#[derive(Clone)]
pub struct A {
    moves: Vec<PossibleMove>,
    start_board: PSBoard,
    single_move_deadline: Duration,
    curr_depth: u8,
    max_search: [f32; 4],
    tx: Sender<bool>,
}

pub struct B {
    moves: Vec<PossibleMove>,
    max_search: [f32; 4],
    continuation: BTreeMap<PossibleMove, PSBoard>,
}

impl Engine {
    pub fn new() -> (Engine, GameState) {
        Engine::with_board_gen(PSBoard::default)
    }

    pub fn from_fen(fen: &str) -> (Engine, GameState) {
        Engine::with_board_gen(|| PSBoard::from_fen(fen))
    }

    fn with_board_gen(initial_board_provider: impl Fn() -> PSBoard) -> (Engine, GameState) {
        let mut sample_board = initial_board_provider();
        let mut moves = Vec::new();
        sample_board.gen_potential_moves(false, &mut moves);
        let scoring_timings = DurationAverage::new(50, || Duration::from_secs(1));
        for mv in 0..scoring_timings.len() as usize {
            scoring_timings.add(Engine::timed_move(&mut sample_board, &moves[mv % moves.len()]).1);
        }
        (
            Engine {
                move_cache: VecCache::new(),
                scoring_timings,
            },
            GameState {
                worked_on_board: initial_board_provider(),
            },
        )
    }

    fn timed_move(board: &PSBoard, amove: &PossibleMove) -> (PSBoard, Duration) {
        let pre = Instant::now();
        (board.make_move_noncached(amove), pre.elapsed())
    }

    fn timing_remembering_move(
        &self,
        board: &PSBoard,
        amove: &PossibleMove,
        tx: Sender<bool>,
    ) -> PSBoard {
        let (ret, dur) = Engine::timed_move(board, amove);
        tx.send(true);
        self.scoring_timings.add(dur);
        ret
    }

    fn similar_quality_moves<'a, F>(
        start_board: &'a PSBoard,
        best_board: &'a PSBoard,
        score_query: F,
    ) -> impl Iterator<Item = &'a PSBoard>
    where
        F: Fn(&PSBoard) -> f32,
    {
        let bb_score = score_query(best_board);
        start_board.continuation.values().filter(move |other| {
            let other_score = score_query(other);
            (other_score.max(bb_score) - other_score.min(bb_score)) < 0.001
        })
    }

    fn select_similar_board<'a, F>(
        start_board: &'a PSBoard,
        best_board: &'a PSBoard,
        score_query: F,
    ) -> &'a PSBoard
    where
        F: Fn(&PSBoard) -> f32,
    {
        let choices = Engine::similar_quality_moves(start_board, best_board, &score_query).count();
        Engine::similar_quality_moves(start_board, best_board, &score_query)
            .nth(thread_rng().gen_range(0..choices))
            .unwrap()
    }

    // board count send instead of lock
    pub fn best_move_for(
        &self,
        state: &mut GameState,
        deadline: &Duration,
    ) -> (Option<PossibleMove>, f32, u32) {
        let (tx, rx) = mpsc::channel();

        let (best_move, score) = self.best_move_for_internal(
            &mut state.worked_on_board,
            deadline,
            Engine::countdown_with_look_ahead,
            0,
            tx.clone(),
        );

        tx.send(false);

        // best move internal is done get result back
        let mut board_count = 0;
        // thread waits for count result
        while rx.recv().unwrap() {
            board_count += 1;
        }
        (best_move, score, board_count)
    }

    pub fn is_mate(score: f32) -> bool {
        (score.abs() - MATE).abs() < 50.0
    }

    // Determines the best move on the depth asked for
    // If the decide flag is passed, we will have the move generated, otherwise we just use this method for scoring
    fn best_move_for_internal<F>(
        &self,
        start_board: &mut PSBoard,
        deadline: &Duration,
        look_ahead_method: F,
        curr_depth: u8,
        sender: Sender<bool>,
    ) -> (Option<PossibleMove>, f32)
    where
        F: Fn(&Self, A) -> B,
    {
        let mut ret = (None, start_board.score);
        let mate_multiplier = match start_board.who_moves {
            White => 1.0,
            Black => -1.0,
        };

        if !Engine::is_mate(start_board.score) {
            let mut moves = self.move_cache.get();
            assert_eq!(0, moves.len());
            start_board.gen_potential_moves(true, &mut moves);
            let movecount = moves.len();
            //println!("Potential moves: {:?}", moves);
            let single_move_deadline = deadline
                .checked_div(movecount as u32)
                .expect("Could not generate a single move!!!");
            // println!("Single move deadline: {:?}", single_move_deadline);

            self.countdown(
                moves,
                start_board,
                single_move_deadline,
                look_ahead_method,
                curr_depth,
                sender,
            );

            let best_potential_board = start_board.continuation.values().max_by(|b1, b2| {
                (mate_multiplier * b1.adjusted_score)
                    .partial_cmp(&(mate_multiplier * b2.adjusted_score))
                    .unwrap_or(Ordering::Less)
            });
            if let Some(best_board) = best_potential_board {
                let selected_board = if best_board.adjusted_score.is_nan() {
                    Engine::select_similar_board(start_board, best_board, |b| b.score)
                } else {
                    Engine::select_similar_board(start_board, best_board, |b| b.adjusted_score)
                };
                ret = start_board
                    .continuation
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

    fn countdown_without_lookahead(&self, mut a: A) -> B {
        let who = a.start_board.who_moves;
        let mut stored_a_mate = false;
        while let Some(curr_move) = a.moves.pop() {
            let mut board_with_move =
                self.timing_remembering_move(&a.start_board, &curr_move, a.tx.clone());
            let average_scoring_duration = self.scoring_timings.calc_average() * 40;
            let curr_score = if !Engine::is_mate(board_with_move.score)
                && (average_scoring_duration < a.single_move_deadline)
            {
                let (_, best_score) = self.best_move_for_internal(
                    &mut board_with_move,
                    &a.single_move_deadline,
                    Engine::countdown_without_lookahead,
                    a.curr_depth + 1,
                    a.tx.clone(),
                );
                best_score
            } else {
                board_with_move.score
            };

            Engine::update_max_search(who, &mut a.max_search, curr_score);

            let mate_detected = Engine::is_mate(curr_score);
            if !stored_a_mate || !mate_detected {
                // We don't store all mates, just the first one we hit,
                // this will allow the score eval to be correct in best_move_for_internal, but
                // we save a lot of memory for all unnecessary continuations

                a.start_board
                    .continuation
                    .insert(curr_move, board_with_move);
                stored_a_mate |= mate_detected;
            }
        }
        B {
            moves: a.moves,
            max_search: a.max_search,
            continuation: a.start_board.continuation,
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

    fn countdown<F>(
        &self,
        moves: Vec<PossibleMove>,
        start_board: &mut PSBoard,
        single_move_deadline: Duration,
        look_ahead_method: F,
        curr_depth: u8,
        tx: Sender<bool>,
    ) where
        F: Fn(&Self, A) -> B,
    {
        start_board.adjusted_score = 0.0;
        let who = start_board.who_moves;
        let max_search = [if who == White {
            f32::NEG_INFINITY
        } else {
            f32::INFINITY
        }; 4];

        let mut tmp_b = look_ahead_method(
            self,
            A {
                moves,
                start_board: start_board.clone(),
                single_move_deadline,
                curr_depth,
                max_search,
                tx,
            },
        );

        start_board.continuation.append(&mut tmp_b.continuation);

        let (moves, mut max_search) = (tmp_b.moves, tmp_b.max_search);
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
        self.move_cache.release(moves);
    }

    fn countdown_with_look_ahead(&self, mut a: A) -> B {
        let who = a.start_board.who_moves;

        let mut joins = Vec::new();

        while let Some(curr_move) = a.moves.pop() {
            let join_a = thread::spawn({
                let board_clone = a.start_board.clone();
                let engine_clone = self.clone();
                let tx = a.tx.clone();

                move || {
                    let mut board_with_move =
                        engine_clone.timing_remembering_move(&board_clone, &curr_move, tx.clone());
                    let (_, curr_score) = engine_clone.best_move_for_internal(
                        &mut board_with_move,
                        &a.single_move_deadline,
                        Engine::countdown_without_lookahead,
                        a.curr_depth + 1,
                        tx.clone(),
                    );

                    (curr_score, board_with_move, curr_move)
                }
            });

            joins.push(join_a);
        }

        for join in joins {
            let (curr_score, board_with_move, curr_move) = join.join().unwrap();
            Engine::update_max_search(who, &mut a.max_search, curr_score);

            println!(
                "{} Evaluated move: {curr_move}, score: {}, adjusted: {}",
                (0..a.curr_depth).map(|_| " ").collect::<String>(),
                board_with_move.score,
                board_with_move.adjusted_score
            );

            a.start_board
                .continuation
                .insert(curr_move, board_with_move);
        }
        B {
            moves: a.moves,
            max_search: a.max_search,
            continuation: a.start_board.continuation,
        }
    }
}

#[cfg(test)]
mod test {
    use std::{sync::mpsc, time::Duration};

    use crate::{
        baserules::board_rep::PossibleMove, engine::Engine, human_facing::helper::find_max_depth,
    };

    /// Test for this game: https://lichess.org/dRlJX08zhn1L
    #[test]
    fn weird_eval_1() {
        let (tx, _rx) = mpsc::channel();
        let (engine, mut gamestate) =
            Engine::from_fen("r1b1kbnr/pppn1ppp/4p3/6q1/4P3/8/PPPP1PPP/RNBQK1NR w KQkq - 0 4");
        let moves = vec![PossibleMove::simple_from_uci("d1h5").unwrap()];
        engine.countdown(
            moves,
            &mut gamestate.worked_on_board,
            Duration::from_millis(100),
            Engine::countdown_with_look_ahead,
            0,
            tx,
        );
        println!("Depth: {}", find_max_depth(gamestate.get_board()));
        println!("{}", gamestate.worked_on_board.adjusted_score);
        assert!(gamestate.worked_on_board.adjusted_score < -4.0);
    }

    /// Test for this game: https://lichess.org/dRlJX08zhn1L
    #[test]
    fn weird_eval_2() {
        let (engine, mut gamestate) =
            Engine::from_fen("r1b1kbnr/pppn1ppp/4p3/6qQ/4P3/8/PPPP1PPP/RNB1K1NR b KQkq - 1 4");
        let (best_move, score, _) =
            engine.best_move_for(&mut gamestate, &Duration::from_millis(100));
        println!("{best_move:?}");
        println!("Depth: {}", find_max_depth(gamestate.get_board()));
        println!("{}", score);
        assert!(score < -6.0);
    }

    /// Test for this game: https://lichess.org/dRlJX08zhn1L
    #[test]
    fn weird_eval_3() {
        let (tx, rx) = mpsc::channel();

        let (engine, mut gamestate) =
            Engine::from_fen("r1b1kbnr/pppn1ppp/4p3/6qQ/4P3/8/PPPP1PPP/RNB1K1NR b KQkq - 1 4");
        let moves = vec![PossibleMove::simple_from_uci("g5d2").unwrap()];
        engine.countdown(
            moves,
            &mut gamestate.worked_on_board,
            Duration::from_millis(100),
            Engine::countdown_with_look_ahead,
            0,
            tx,
        );
        println!("Depth: {}", find_max_depth(gamestate.get_board()));
        println!("{}", gamestate.worked_on_board.adjusted_score);
        assert!(gamestate.worked_on_board.adjusted_score > 2.0);
    }

    /// Test for this game: https://lichess.org/ZnIAbaQXqHCF
    #[test]
    fn weird_eval_4() {
        let (tx, rx) = mpsc::channel();

        let (engine, mut gamestate) =
            Engine::from_fen("r2qk2r/pp1nbppp/2p5/5b2/4p3/PQ6/1P1PPPPP/R1B1KBNR w KQkq - 4 11");
        let moves = vec![PossibleMove::simple_from_uci("b3f7").unwrap()];
        engine.countdown(
            moves,
            &mut gamestate.worked_on_board,
            Duration::from_millis(1000),
            Engine::countdown_with_look_ahead,
            0,
            tx,
        );
        println!("Depth: {}", find_max_depth(gamestate.get_board()));
        println!("{}", gamestate.worked_on_board.adjusted_score);
        assert!(gamestate.worked_on_board.adjusted_score < -5.0);
    }

    /// Test for this game: https://lichess.org/ZnIAbaQXqHCF
    #[test]
    fn weird_eval_4_1() {
        let (engine, mut gamestate) =
            Engine::from_fen("r2qk2r/pp1nbppp/2p5/5b2/4p3/PQ6/1P1PPPPP/R1B1KBNR w KQkq - 4 11");
        let (best, score, _) = engine.best_move_for(&mut gamestate, &Duration::from_millis(2000));
        println!("Depth: {}", find_max_depth(gamestate.get_board()));
        println!("Best move: {best:?} Eval: {score}");
        assert!(!best
            .unwrap()
            .eq(&PossibleMove::simple_from_uci("b3f7").unwrap()));
    }

    /// Test for this game: https://lichess.org/5fa8V5PVXDEL
    #[test]
    fn weird_eval_5() {
        let (engine, mut gamestate) =
            Engine::from_fen("2b2rk1/p2p1ppp/8/P7/R2PPP2/8/1r1K2PP/5R2 w - - 0 26");
        let (best, score, _) = engine.best_move_for(&mut gamestate, &Duration::from_millis(20000));
        println!("Best move: {best:?} Eval: {score}");
        println!("Depth: {}", find_max_depth(gamestate.get_board()));
        gamestate.make_a_human_move("Kc3").unwrap();
        gamestate.make_a_human_move("Rxg2").unwrap();
        let (best, score, _) = engine.best_move_for(&mut gamestate, &Duration::from_millis(8000));
        println!("Best move: {best:?} Eval: {score}");
        println!("Depth: {}", find_max_depth(gamestate.get_board()));
        gamestate.make_a_human_move("Rb1").unwrap();
        gamestate.make_a_human_move("Rxh2").unwrap();
        let (best, score, _) = engine.best_move_for(&mut gamestate, &Duration::from_millis(8000));
        println!("Best move: {best:?} Eval: {score}");
        println!("Depth: {}", find_max_depth(gamestate.get_board()));
        gamestate.make_a_human_move("Rb8").unwrap();
        gamestate.make_a_human_move("Re8").unwrap();
        let (best, score, _) = engine.best_move_for(&mut gamestate, &Duration::from_millis(8000));
        println!("Best move: {best:?} Eval: {score}");
        println!("Depth: {}", find_max_depth(gamestate.get_board()));
        gamestate.make_a_human_move("Rc4").unwrap();
        gamestate.make_a_human_move("d6").unwrap();
        let (best, score, _) = engine.best_move_for(&mut gamestate, &Duration::from_millis(8000));
        println!("Best move: {best:?} Eval: {score}");
        println!("Depth: {}", find_max_depth(gamestate.get_board()));
        gamestate.make_a_human_move("Rcxc8").unwrap();
        gamestate.make_a_human_move("Rh3+").unwrap();
        let (best, score, _) = engine.best_move_for(&mut gamestate, &Duration::from_millis(8000));
        println!("Best move: {best:?} Eval: {score}");
        let acceptable_moves = [
            PossibleMove::simple_from_uci("c3c2").unwrap(),
            PossibleMove::simple_from_uci("c3d2").unwrap(),
            PossibleMove::simple_from_uci("c3b2").unwrap(),
            PossibleMove::simple_from_uci("c3c4").unwrap(),
            PossibleMove::simple_from_uci("c3b4").unwrap(),
        ];
        println!("Depth: {}", find_max_depth(gamestate.get_board()));
        let the_move = best.unwrap();
        assert!(acceptable_moves
            .iter()
            .any(|acceptable| acceptable.eq(&the_move)));
    }
}
