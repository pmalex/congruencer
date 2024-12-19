//! Этот модуль предназначен для описания тождеств, которые затем будут проверяться
//! на произвольной решётке. И также это нужно для поиска тождеств, выполняющихся
//! на произвольной решётке.

use crate::{magma::Magma, ElementIndex};

pub enum Expression {
    Variable(char),
    Node(Box<Expression>, Magma, Box<Expression>),
}

impl Expression {
    pub fn evaluate(&self) -> ElementIndex {
        todo!()
    }
}
