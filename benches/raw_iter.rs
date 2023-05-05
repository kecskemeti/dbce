use criterion::{black_box, criterion_group, criterion_main, Criterion};
use dbce::baserules::board::PSBoard;

fn raw_iter(c: &mut Criterion) {
    let board = PSBoard::default().board;
    c.bench_function("RawBoard-Iterate", |b| {
        b.iter(|| black_box(board).into_iter().filter(|p| p.is_none()).count())
    });
}

criterion_group!(rit, raw_iter);
criterion_main!(rit);
