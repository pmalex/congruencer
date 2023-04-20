use congruencer::{
    partition::{self, Partition},
    poset,
};

/// Print a partitions set in the Graphiz Dot format.
fn partitions_set_print_dot(partitions_set: &[Partition], alphabet: &str) {
    println!("graph lattice {{");
    println!("\trankdir = TB;");
    println!("\tratio = 0.75;");
    println!("\tnode[shape = none];");
    println!();

    for partition in partitions_set {
        for nearest_upper_bound in
            poset::nearest_incomparable_lower_bounds(partitions_set, partition)
        {
            print!("\t\"");
            partition.print(alphabet);
            print!("\" -- \"");
            nearest_upper_bound.print(alphabet);
            println!("\"");
        }
    }

    println!("}}");
}

fn main() {
    // partitions_set_print_dot(&partition::new_partitions_set(4), "abcd");
    partitions_set_print_dot(&partition::new_partitions_set(5), "abcde");
}
