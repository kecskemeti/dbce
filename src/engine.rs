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
use crate::util::DurationAverage;
use std::cmp::Ordering;
use std::ptr;
use std::sync::atomic::AtomicU8;
use std::sync::atomic::Ordering::Acquire;
use std::sync::{Arc, Mutex};

use crate::baserules::rawboard::is_mate;
use crate::engine::continuation::BoardContinuation;
use crate::engine::enginethread::EngineThread;
use crate::engine::gamestate::GameState;
use async_scoped::TokioScope;
use async_trait::async_trait;
use global_counter::primitive::fast::FlushingCounterU32;
use tokio::spawn;
use tokio::sync::RwLock;
use tokio::time::sleep;

use std::time::Duration;

#[derive(Clone)]
pub struct Engine {
    scoring_timings: Arc<Mutex<DurationAverage>>,
    exploration_allowed: Arc<RwLock<bool>>,
    thread_counter: Arc<AtomicU8>,
}

#[async_trait]
trait DepthsBoardCountMaintenance<T> {
    async fn best_move_for(self, board_count: &FlushingCounterU32, depth: &AtomicU8) -> T;
}

struct SeqEngine(Engine);
struct ParEngine(Engine);

#[async_trait]
trait Explore: Send + Sync {
    async fn explore<'a>(&'a self, explore: ExplorationInput<'a>) -> ExplorationOutput;
}

#[async_trait]
impl Explore for SeqEngine {
    async fn explore<'a>(&'a self, mut a: ExplorationInput<'a>) -> ExplorationOutput {
        let who = a.start_board.who_moves;
        while let Some(curr_move) = a.moves.pop() {
            let board_with_move =
                a.thread_info
                    .timing_remembering_move(a.start_board, &curr_move, a.counter);
            // wait for barrier completion instead of yield
            tokio::task::yield_now().await;
            let explore_allowed = { self.0.exploration_allowed.read().await.clone() };
            let curr_score =
                if !is_mate(board_with_move.score) && explore_allowed && a.curr_depth < 10 {
                    let (_, best_score) = self
                        .0
                        .best_move_for_internal(
                            board_with_move,
                            a.curr_depth + 1,
                            a.counter,
                            a.maximum,
                            a.thread_info,
                        )
                        .await;
                    best_score
                } else {
                    let mut val_before = a.maximum.load(Acquire);
                    loop {
                        let cur_max = a.maximum.fetch_max(a.curr_depth, Acquire);
                        if cur_max == a.curr_depth {
                            let res = a
                                .maximum
                                .compare_exchange(val_before, cur_max, Acquire, Acquire);

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
}

#[async_trait]
impl Explore for ParEngine {
    async fn explore<'a>(&'a self, mut a: ExplorationInput<'a>) -> ExplorationOutput {
        let (_, joins) = TokioScope::scope_and_block(|s| {
            // copy

            while let Some(curr_move) = a.moves.pop() {
                s.spawn(Self::abc(
                    a.thread_info.clone(),
                    a.start_board.clone(),
                    self.0.clone(),
                    curr_move,
                    a.counter,
                    a.maximum,
                    a.curr_depth,
                ));
            }
        });
        let who = a.start_board.who_moves;
        for join in joins {
            let (curr_score, curr_move, timing_average, board_clone): (
                f32,
                PossibleMove,
                Duration,
                BoardContinuation,
            ) = join.unwrap();
            Engine::update_max_search(who, &mut a.max_search, curr_score);
            {
                let mut global_timings = self.0.scoring_timings.lock().unwrap();
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
    }
}

impl ParEngine {
    async fn abc(
        mut thread_info_clone: EngineThread,
        mut board_clone: BoardContinuation,
        engine_clone: Engine,
        curr_move: PossibleMove,
        counter: &FlushingCounterU32,
        maximum: &AtomicU8,
        curr_depth: u8,
    ) -> (f32, PossibleMove, Duration, BoardContinuation) {
        let prev = engine_clone.thread_counter.fetch_add(1, Acquire);
        if prev > 150 {
            panic!("out of control!");
        }
        let board_with_move =
            thread_info_clone.timing_remembering_move(&mut board_clone, &curr_move, counter);
        let (_, curr_score) = engine_clone
            .best_move_for_internal(
                board_with_move,
                curr_depth + 1,
                counter,
                maximum,
                &mut thread_info_clone,
            )
            .await;
        counter.flush();
        engine_clone.thread_counter.fetch_sub(1, Acquire);
        (
            curr_score,
            curr_move,
            thread_info_clone.0.calc_average(),
            board_clone,
        )
    }
}

pub struct ExplorationInput<'a> {
    moves: Vec<PossibleMove>,
    start_board: &'a mut BoardContinuation,
    curr_depth: u8,
    max_search: [f32; 4],
    counter: &'a FlushingCounterU32,
    maximum: &'a AtomicU8,
    thread_info: &'a mut EngineThread,
}

pub struct ExplorationOutput {
    max_search: [f32; 4],
}

struct ExtEngine<'a>(Engine, &'a mut BoardContinuation);

#[async_trait]
impl<'a> DepthsBoardCountMaintenance<(Option<PossibleMove>, f32)> for ExtEngine<'a> {
    async fn best_move_for(
        mut self,
        board_count: &FlushingCounterU32,
        depth: &AtomicU8,
    ) -> (Option<PossibleMove>, f32) {
        self.0
            .best_move_for_internal(
                self.1,
                0,
                board_count,
                depth,
                &mut EngineThread::from(&self.0),
            )
            .await
    }
}

impl Engine {
    pub fn new() -> (Self, GameState) {
        Self::with_board_gen(PSBoard::default)
    }

    async fn time_up(&self, duration: Duration) {
        sleep(duration).await;
        let mut guard = self.exploration_allowed.write().await;
        *guard = false;
    }

    pub fn from_fen(fen: &str) -> (Self, GameState) {
        Self::with_board_gen(|| PSBoard::from_fen(fen).expect("Incorrect fen input"))
    }

    fn par_explore(&self) -> ParEngine {
        ParEngine(self.clone())
    }

    fn seq_explore(&self) -> SeqEngine {
        SeqEngine(self.clone())
    }

    fn with_board_gen(initial_board_provider: impl Fn() -> PSBoard) -> (Self, GameState) {
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
            Self {
                scoring_timings: Arc::new(Mutex::new(scoring_timings)),
                exploration_allowed: Arc::new(RwLock::new(true)),
                thread_counter: Arc::new(AtomicU8::new(0)),
            },
            GameState {
                worked_on_board: BoardContinuation::new(initial_board_provider()),
            },
        )
    }

    async fn manage_counter<T>(to_count: impl DepthsBoardCountMaintenance<T>) -> (T, u32, u8) {
        let board_counter = FlushingCounterU32::new(0);
        let maximum_depth = AtomicU8::new(0);
        let ret = to_count.best_move_for(&board_counter, &maximum_depth).await;
        board_counter.flush();
        (ret, board_counter.get(), maximum_depth.load(Acquire))
    }

    // board count send instead of lock
    pub async fn best_move_for(
        &self,
        state: &mut GameState,
        duration: &Duration,
    ) -> (Option<PossibleMove>, f32, u32, u8) {
        let engine_clone = self.clone();
        let duration_clone = duration.clone();

        spawn(async move { engine_clone.time_up(duration_clone).await });
        let ((best_move, score), board_count, maximum) =
            Self::manage_counter(ExtEngine(self.clone(), &mut state.worked_on_board)).await;

        (best_move, score, board_count, maximum)
    }

    // Determines the best move on the depth asked for
    // If the decide flag is passed, we will have the move generated, otherwise we just use this method for scoring
    async fn best_move_for_internal(
        &self,
        start_board: &mut BoardContinuation,
        curr_depth: u8,
        counter: &FlushingCounterU32,
        maximum: &AtomicU8,
        thread_info: &mut EngineThread,
    ) -> (Option<PossibleMove>, f32) {
        let mut ret = (None, start_board.score);
        let mate_multiplier = start_board.who_moves.mate_multiplier();

        if !is_mate(start_board.score) {
            let mut moves = Vec::new();
            start_board.gen_potential_moves(&mut moves);

            //println!("Potential moves: {:?}", moves);
            let exploration_method: Box<dyn Explore> = if self.thread_counter.load(Acquire) < 5 {
                Box::new(self.par_explore())
            } else {
                Box::new(self.seq_explore())
            };

            self.exploration(
                moves,
                start_board,
                exploration_method,
                curr_depth,
                counter,
                maximum,
                thread_info,
            )
            .await;

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
                            Some((Some(*amove), selected_board.score()))
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

    fn update_max_search(who: PieceColor, max_search: &mut [f32], curr_score: f32) {
        max_search.sort_unstable_by(who.score_comparator());
        for (idx, a_good_score) in max_search.iter().enumerate() {
            if who.is_better_score(*a_good_score, curr_score) {
                max_search[idx] = curr_score;
                break;
            }
        }
    }

    async fn exploration(
        &self,
        moves: Vec<PossibleMove>,
        start_board: &mut BoardContinuation,
        exploration_helper: Box<dyn Explore>,
        curr_depth: u8,
        counter: &FlushingCounterU32,
        maximum: &AtomicU8,
        thread_info: &mut EngineThread,
    ) {
        start_board.adjusted_score = 0.0;
        let who = start_board.who_moves;
        let max_search = [who.worst_score(); 4];

        let mut max_search = exploration_helper
            .explore(ExplorationInput {
                moves,
                start_board,

                curr_depth,
                max_search,
                counter,
                maximum,
                thread_info,
            })
            .await
            .max_search;

        max_search.sort_unstable_by(who.score_comparator());
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
}

#[cfg(test)]
mod test {
    use std::time::Duration;

    use super::continuation::BoardContinuation;
    use super::DepthsBoardCountMaintenance;
    use crate::baserules::board::PSBoard;
    use crate::engine::{EngineThread, GameState};
    use crate::human_facing::helper;
    use crate::{baserules::board_rep::PossibleMove, engine::Engine};
    use async_trait::async_trait;
    use global_counter::primitive::fast::FlushingCounterU32;
    use std::sync::atomic::AtomicU8;
    use tokio::spawn;
    use tokio::test;

    /// Test for this game: https://lichess.org/dRlJX08zhn1L
    #[test(flavor = "multi_thread")]
    async fn weird_eval_1() {
        let (engine, mut gamestate) =
            Engine::from_fen("r1b1kbnr/pppn1ppp/4p3/6q1/4P3/8/PPPP1PPP/RNBQK1NR w KQkq - 0 4");
        let moves = vec![PossibleMove::simple_from_uci("d1h5").unwrap()];

        let result = Engine::manage_counter(ExploreHelper(
            engine.clone(),
            moves,
            &mut gamestate.worked_on_board,
            EngineThread::from(&engine),
        ))
        .await;
        println!("Depth: {}", result.2);
        println!("{}", gamestate.worked_on_board.adjusted_score);
        assert!(gamestate.worked_on_board.adjusted_score < -4.0);
    }

    /// Test for this game: https://lichess.org/dRlJX08zhn1L
    #[test(flavor = "multi_thread")]
    async fn weird_eval_2() {
        let (engine, mut gamestate) =
            Engine::from_fen("r1b1kbnr/pppn1ppp/4p3/6qQ/4P3/8/PPPP1PPP/RNB1K1NR b KQkq - 1 4");
        let (_, (_, score)) =
            helper::calculate_move_for_console(&engine, &mut gamestate, &Duration::from_secs(2))
                .await;
        assert!(score < -6.0);
    }

    struct ExploreHelper<'a>(
        Engine,
        Vec<PossibleMove>,
        &'a mut BoardContinuation,
        EngineThread,
    );

    #[async_trait]
    impl<'a> DepthsBoardCountMaintenance<()> for ExploreHelper<'a> {
        async fn best_move_for(mut self, board_count: &FlushingCounterU32, depth: &AtomicU8) {
            let engine_clone = self.0.clone();

            spawn(async move { engine_clone.time_up(Duration::from_millis(500)).await });
            self.0
                .exploration(
                    self.1,
                    &mut self.2,
                    Box::new(self.0.par_explore()),
                    0,
                    board_count,
                    depth,
                    &mut self.3,
                )
                .await
        }
    }

    /// Test for this game: https://lichess.org/dRlJX08zhn1L
    #[test(flavor = "multi_thread")]
    async fn weird_eval_3() {
        let (engine, mut gamestate) =
            Engine::from_fen("r1b1kbnr/pppn1ppp/4p3/6qQ/4P3/8/PPPP1PPP/RNB1K1NR b KQkq - 1 4");
        let moves = vec![PossibleMove::simple_from_uci("g5d2").unwrap()];

        let result = Engine::manage_counter(ExploreHelper(
            engine.clone(),
            moves,
            &mut gamestate.worked_on_board,
            EngineThread::from(&engine),
        ))
        .await;
        println!("Depth: {}", result.2);
        println!("{}", gamestate.worked_on_board.adjusted_score);
        assert!(gamestate.worked_on_board.adjusted_score > 2.0);
    }

    /// Test for this game: https://lichess.org/ZnIAbaQXqHCF
    #[test(flavor = "multi_thread")]
    async fn weird_eval_4() {
        let (engine, mut gamestate) =
            Engine::from_fen("r2qk2r/pp1nbppp/2p5/5b2/4p3/PQ6/1P1PPPPP/R1B1KBNR w KQkq - 4 11");
        let moves = vec![PossibleMove::simple_from_uci("b3f7").unwrap()];

        let result = Engine::manage_counter(ExploreHelper(
            engine.clone(),
            moves,
            &mut gamestate.worked_on_board,
            EngineThread::from(&engine),
        ))
        .await;
        println!("Depth: {}", result.2);
        println!("{}", gamestate.worked_on_board.adjusted_score);
        assert!(gamestate.worked_on_board.adjusted_score < -5.0);
    }

    /// Test for this game: https://lichess.org/ZnIAbaQXqHCF
    #[test(flavor = "multi_thread")]
    async fn weird_eval_4_1() {
        let (engine, mut gamestate) =
            Engine::from_fen("r2qk2r/pp1nbppp/2p5/5b2/4p3/PQ6/1P1PPPPP/R1B1KBNR w KQkq - 4 11");
        let (_, (best, _)) =
            helper::calculate_move_for_console(&engine, &mut gamestate, &Duration::from_secs(2))
                .await;
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
        async fn build_continuation_and_move(
            &self,
            gamestate: &mut GameState,
            deadline: &Duration,
            engine_move: &str,
            opponent_move: &str,
        ) {
            helper::calculate_move_for_console(self, gamestate, deadline).await;
            gamestate.make_a_move_pair(engine_move, opponent_move);
        }
    }

    /// Test for this game: https://lichess.org/5fa8V5PVXDEL
    #[test(flavor = "multi_thread")]
    async fn weird_eval_5() {
        let (engine, mut gamestate) =
            Engine::from_fen("2b2rk1/p2p1ppp/8/P7/R2PPP2/8/1r1K2PP/5R2 w - - 0 26");
        let initial_duration = Duration::from_secs(20);
        engine
            .build_continuation_and_move(&mut gamestate, &initial_duration, "Kc3", "Rxg2")
            .await;
        let normal_duration = Duration::from_secs(8);
        engine
            .build_continuation_and_move(&mut gamestate, &normal_duration, "Rb1", "Rxh2")
            .await;
        engine
            .build_continuation_and_move(&mut gamestate, &normal_duration, "Rb8", "Re8")
            .await;
        engine
            .build_continuation_and_move(&mut gamestate, &normal_duration, "Rc4", "d6")
            .await;
        engine
            .build_continuation_and_move(&mut gamestate, &normal_duration, "Rcxc8", "Rh3+")
            .await;
        let (_, (best, _)) =
            helper::calculate_move_for_console(&engine, &mut gamestate, &normal_duration).await;
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
    async fn prep_failed_game_4() -> (Engine, GameState) {
        let (engine, mut gamestate) =
            Engine::from_fen("rn2kbnr/p1q1pNpp/1pp1P3/3p4/8/2N5/PPP1QPPP/R1B1KR2 b Qkq - 2 11");
        let normal_duration = Duration::from_secs(2);
        engine
            .build_continuation_and_move(&mut gamestate, &normal_duration, "d4", "Nxh8")
            .await;
        engine
            .build_continuation_and_move(&mut gamestate, &normal_duration, "dxc3", "Qh5+")
            .await;
        engine
            .build_continuation_and_move(&mut gamestate, &normal_duration, "Kd8", "Nf7+")
            .await;
        (engine, gamestate)
    }

    /// Tests for this game: https://lichess.org/73Bl5rBonV45
    #[test(flavor = "multi_thread")]
    async fn failed_game_4() {
        let (engine, mut gamestate) = prep_failed_game_4().await;
        let (_, move_to_do) =
            helper::calculate_move_for_console(&engine, &mut gamestate, &Duration::from_secs(5))
                .await;
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
    #[test(flavor = "multi_thread")]
    async fn failed_game_4_subtest() {
        let (engine, mut gamestate) = prep_failed_game_4().await;
        helper::calculate_move_for_console(&engine, &mut gamestate, &Duration::from_secs(1)).await;
        gamestate.make_a_human_move_or_panic("cxb2");
        let the_board = gamestate.continuation().clone();
        let mut moves = Vec::new();
        the_board.gen_potential_moves(&mut moves);
        println!("{moves:?}");
        assert!(moves.contains(&PossibleMove::simple_from_uci("f7d8").unwrap()));
    }

    #[test(flavor = "multi_thread")]
    async fn retain_boards() {
        let (engine, mut gamestate) = Engine::from_fen("8/8/8/8/6PP/6Pk/7P/7K w - - 0 1");
        let short_deadline = Duration::from_millis(1);
        let (_, _, _, depth) = engine.best_move_for(&mut gamestate, &short_deadline).await;
        println!("Max Depth: {depth}");
        gamestate.worked_on_board.visualise_explored_moves();
        let a_selected_move = *gamestate.worked_on_board.keys().next().unwrap();
        println!("selected move {:?}", a_selected_move);
        let the_selected_board = gamestate
            .worked_on_board
            .find_continuation(&a_selected_move)
            .unwrap();
        let continuations_before = the_selected_board.total_continuation_boards();
        gamestate.make_a_generated_move(&a_selected_move);
        let (_, _, board_count, depth) =
            engine.best_move_for(&mut gamestate, &short_deadline).await;
        println!("Max Depth: {depth}");
        gamestate.worked_on_board.visualise_explored_moves();
        let continuations_after = gamestate.worked_on_board.total_continuation_boards();
        assert_eq!(continuations_after - continuations_before, board_count);
    }

    /// Tests for this game: https://lichess.org/NPchEbrvI0qD
    #[test(flavor = "multi_thread")]
    async fn failed_game_1() {
        let board = PSBoard::from_fen("5rk1/2q2p1p/5Q2/3p4/1P2p1bP/P3P3/5PP1/R1r1K1NR w KQ - 1 26")
            .unwrap();
        let (engine, mut gamestate) =
            Engine::from_fen("5rk1/2q2p1p/5Q2/3p4/1P2p1bP/P3P3/2r2PP1/R3K1NR b KQ - 0 25");

        gamestate.make_a_human_move("Rc1+").unwrap();
        assert_eq!(format!("{board}"), format!("{}", gamestate.psboard()));

        let move_to_do = engine
            .best_move_for(&mut gamestate, &Duration::from_millis(1))
            .await;
        assert_eq!(
            PossibleMove::simple_from_uci("a1c1").unwrap(),
            move_to_do.0.unwrap()
        );
    }

    /// Tests for this game: https://lichess.org/DbFqFBaYGgr6
    #[test(flavor = "multi_thread")]
    async fn failed_game_2() {
        let board =
            PSBoard::from_fen("rnbk3r/1p1p3p/5Q1n/2N2P2/p7/8/PPP2KPP/R1B2B1R b - - 0 14").unwrap();
        let (engine, mut gamestate) =
            Engine::from_fen("rnbk3r/1p1p3p/3Q1p1n/2N2P2/p7/8/PPP2KPP/R1B2B1R w - - 4 14");

        gamestate.make_a_human_move("Qxf6+").unwrap();
        assert_eq!(format!("{board}"), format!("{}", gamestate.psboard()));

        let move_to_do = engine
            .best_move_for(&mut gamestate, &Duration::from_millis(1))
            .await;
        let acceptable_moves = [
            PossibleMove::simple_from_uci("d8c7").unwrap(),
            PossibleMove::simple_from_uci("d8e8").unwrap(),
        ];
        let the_move = move_to_do.0.unwrap();
        assert!(acceptable_moves
            .iter()
            .any(|acceptable| acceptable.eq(&the_move)));
    }

    /// Tests for this game: https://lichess.org/9KuuHpmFX74q
    /// Ideally, this test should not have such a long deadline that we have now
    #[test(flavor = "multi_thread")]
    async fn failed_game_3() {
        let board = PSBoard::from_fen(
            "1rbq1knr/1npp2Q1/p4P1p/1p1P4/1P1B2p1/N2B4/P1P2PPP/1R3RK1 b - - 1 23",
        )
        .unwrap();
        let (engine, mut gamestate) =
            Engine::from_fen("1rbq1knr/1npp4/p4PQp/1p1P4/1P1B2p1/N2B4/P1P2PPP/1R3RK1 w - - 0 23");

        gamestate.make_a_human_move("Qg7+").unwrap();
        assert_eq!(format!("{board}"), format!("{}", gamestate.psboard()));

        let move_to_do = engine
            .best_move_for(&mut gamestate, &Duration::from_millis(1))
            .await;
        assert_eq!(
            PossibleMove::simple_from_uci("f8e8").unwrap(),
            move_to_do.0.unwrap()
        );
    }
}
