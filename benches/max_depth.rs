use criterion::{black_box, criterion_group, criterion_main, Criterion};
use dbce::engine::Engine;
use dbce::human_facing::helper::find_max_depth;
use std::time::Duration;

fn criterion_benchmark(c: &mut Criterion) {
    let (engine, mut gamestate) =
        Engine::from_fen("2b2rk1/p2p1ppp/8/P7/R2PPP2/8/1r1K2PP/5R2 w - - 0 26");
    engine.best_move_for(&mut gamestate, &Duration::from_secs(5));
    c.bench_function("Engine-Weird5-5sec", |b| {
        b.iter(|| find_max_depth(black_box(gamestate.get_board())))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
