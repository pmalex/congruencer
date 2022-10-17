fn main() {
    let vv = congruencer::generate_partitions(13);

    // for partition in &vv {
    //     print!("{}, ", partition);
    // }
    // println!();

    println!("{}", vv.len());
}
