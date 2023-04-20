use congruencer::binary_operation::iterator::BinaryOperationsIterator;

fn main() {
    let it = BinaryOperationsIterator::new(3).filter(|f| f.is_commutative());

    for f in it {
        println!("{}", f);
    }
}
