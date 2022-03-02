mod calculus;
mod computation;
mod equalities;
mod fold_addition;
pub mod fold_multiplication;
mod normalize;

use std::{collections::HashSet, fmt::Debug, time::Instant};

use itertools::Itertools;

use crate::{expression::Expression, matchh::MatchResult, number::Number};

pub trait Simplifier: Debug {
    fn apply(&self, to: &mut Expression);
}

macro_rules! simplifiers {
    ($($e:expr,)*) => {
        vec![$(Box::new($e)),*]
    }
}

pub type Simplifiers = Vec<Box<dyn Simplifier>>;

pub fn prepass() -> Simplifiers {
    simplifiers![normalize::SNormalize,]
}

pub fn expand() -> Simplifiers {
    simplifiers![]
}

pub fn intermediates() -> Vec<Box<dyn Simplifier>> {
    simplifiers![
        computation::ScAdd,
        computation::ScDiv,
        computation::ScMul,
        computation::ScNeg,
        computation::ScPow,
        computation::ScSub,
        //
        equalities::SMulIdentity,
        equalities::SMulZero,
        equalities::SPowIdentity,
        equalities::SPowZero,
        //
        calculus::SdAddRule,
        calculus::SdConstant,
        calculus::SdDivRule,
        calculus::SdIdentical,
        calculus::SdMulRule,
        calculus::SdSubRule,
    ]
}

pub fn collapse() -> Simplifiers {
    simplifiers![
        fold_multiplication::SFoldMultiplication,
        fold_addition::SFoldAddition,
    ]
}
