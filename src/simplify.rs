mod collapse;
mod expand;
mod intermediates;
mod prepass;

use std::fmt::Debug;

pub use self::{
    collapse::simplifiers as collapse,
    expand::simplifiers as expand,
    intermediates::simplifiers as intermediates,
    prepass::{simplifiers as prepass, SNormalize},
};
use crate::expression::Expression;

pub trait Simplifier: Debug {
    fn apply(&self, to: &mut Expression);
}

#[macro_export]
macro_rules! simplifiers {
    ($($e:expr,)*) => {
        vec![$(Box::new($e)),*]
    }
}

pub type Simplifiers = Vec<Box<dyn Simplifier>>;
