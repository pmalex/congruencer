use crate::{binary_operation::dynamic::BinaryOperation, ElementIndex};

pub enum Expression {
    Variable(char),
    Node(Box<Expression>, BinaryOperation, Box<Expression>),
}

impl Expression {
    pub fn evaluate(&self) -> ElementIndex {
        todo!()
    }
}
