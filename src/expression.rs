use crate::binary_operation::{BinaryOperation, Element};

pub enum Expression {
    Variable(char),
    Node(Box<Expression>, BinaryOperation, Box<Expression>),
}

impl Expression {
    pub fn evaluate(&self) -> Element {
        todo!()
    }
}
