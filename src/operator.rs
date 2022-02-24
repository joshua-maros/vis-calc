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

use crate::{expression::Expression, number::Number};

pub struct Operator {
    pub name: String,
    pub num_args: usize,
}

impl Debug for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Operator")
            .field("name", &self.name)
            .field("num_args", &self.num_args)
            .finish()
    }
}

impl Operator {
    fn new(name: &str, num_args: usize) -> Self {
        Self {
            name: name.to_owned(),
            num_args,
        }
    }

    pub fn truee() -> Self {
        Self::new("true", 0)
    }

    pub fn falsee() -> Self {
        Self::new("false", 0)
    }

    pub fn and() -> Self {
        Self::new("and", 2)
    }

    pub fn or() -> Self {
        Self::new("or", 2)
    }

    pub fn not() -> Self {
        Self::new("not", 1)
    }

    pub fn implies() -> Self {
        Self::new("imp", 2)
    }

    pub fn biconditional() -> Self {
        Self::new("bicond", 2)
    }

    pub fn equal() -> Self {
        Self::new("eq", 2)
    }

    pub fn not_equal() -> Self {
        Self::new("neq", 2)
    }

    pub fn add() -> Self {
        Self::new("add", 2)
    }

    pub fn sub() -> Self {
        Self::new("sub", 2)
    }

    pub fn neg() -> Self {
        Self::new("neg", 1)
    }

    pub fn mul() -> Self {
        Self::new("mul", 2)
    }

    pub fn div() -> Self {
        Self::new("div", 2)
    }

    pub fn modd() -> Self {
        Self::new("mod", 2)
    }

    pub fn pow() -> Self {
        Self::new("pow", 2)
    }
}
