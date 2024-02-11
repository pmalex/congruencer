//! Последовательный перебор и распечатка ассоциативных и коммутативных бинарных операций.

use congruencer::binary_operation::dynamic::iterator::BinaryOperationIterator;

fn main() {
    // Итератор будет перебирать всевозможные бинарные операции, состоящие из 4 элементов.
    let it =
        BinaryOperationIterator::new(4).filter(|f| f.is_associative() && f.is_commutative());

    // Так как число всевозможных бинарных операций из 4 элементов равно 4^4 (что довольно много),
    // то для экономии времени распечатаем лишь первые 10 ассоциативных и коммутативных.
    for f in it.take(10) {
        println!("{}", f);
    }
}
