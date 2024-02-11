//! Пример построения решётки всевозможных разбиений конечного множества и вывода её в формате Graphiz Dot.

use congruencer::{
    partition::{self, Partition},
    poset,
};

/// Print a partitions set in the Graphiz Dot format.
fn partitions_set_print_dot(partitions_set: &[Partition]) {
    println!("graph lattice {{");
    println!("\trankdir = TB;");
    println!("\tratio = 0.75;");
    println!("\tnode[shape = none];");
    println!();

    for partition in partitions_set {
        for nearest_upper_bound in
            poset::nearest_incomparable_lower_bounds(partitions_set, partition)
        {
            println!("\t\"{}\" -- \"{}\"", partition, nearest_upper_bound)
        }
    }

    println!("}}");
}

fn main() {
    partitions_set_print_dot(&partition::new_partitions_set(&["a", "b", "c", "d", "e"]));
}
