use congruencer::{lattice::Lattice, partition::Partition};

fn main() {
    let partition_set = Partition::new_partition_set(5);

    for p in &partition_set {
        print!("{} ", p);
    }
    println!();

    let lattice = Lattice::from(partition_set.as_slice());

    lattice.print_dot(&partition_set);
}
