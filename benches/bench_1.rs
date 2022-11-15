use congruencer::{partition::Partition, poset::nearest_incomparable_upper_bounds};
use criterion::{criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    let partition_set = Partition::new_partition_set(8);

    c.bench_function("Lattice", |b| {
        b.iter(|| {
            nearest_incomparable_upper_bounds(
                &partition_set,
                &partition_set[partition_set.len() - 1],
            )
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
