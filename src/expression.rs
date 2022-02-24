use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Debug, Display, Formatter},
    hash::{Hash, Hasher},
    ops::{Div, DivAssign, Mul, MulAssign, Rem, RemAssign},
    time::Instant,
};

use derive_more::*;
use itertools::Itertools;
use maplit::hashmap;

use crate::number::Number;

#[macro_export]
macro_rules! make_expr{
    (($name:ident $($args:tt)*)) => {
        Expression::Operator(stringify!($name).to_owned(), vec![$(make_expr!($args)),*])
    };
    ($name:ident) => {
        Expression::Variable(stringify!($name).to_owned())
    };
    ($number:literal) => {
        Expression::Number($number.into())
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Expression {
    Number(Number),
    Operator(String, Vec<Expression>),
    Variable(String),
}

impl Debug for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        <Self as Display>::fmt(&self, f)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(num) => write!(f, "{}", num),
            Self::Operator(name, args) => {
                write!(f, "({}", name)?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
                write!(f, ")")
            }
            Self::Variable(name) => write!(f, "{}", name),
        }
    }
}

impl Expression {
    fn as_number(&self) -> Option<&Number> {
        if let Self::Number(v) = self {
            Some(v)
        } else {
            None
        }
    }
}
