use congruencer::binary_operation::BinaryOperation;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("BinaryOperation(20)", |b| {
        b.iter(|| BinaryOperation::new(black_box(20)).is_commutative_new())
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
