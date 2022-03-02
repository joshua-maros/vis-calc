mod calculus;
mod computation;
mod equalities;
mod fold_addition;
mod normalize;
pub mod fold_multiplication;

use std::{collections::HashSet, fmt::Debug, time::Instant};

use itertools::Itertools;

use crate::{expression::Expression, matchh::MatchResult, number::Number};

pub trait Simplifier: Debug {
    fn apply(&self, to: &mut Expression);
}

pub fn simplifiers() -> Vec<Box<dyn Simplifier>> {
    macro_rules! simplifiers {
        ($($e:expr,)*) => {
            vec![$(Box::new($e)),*]
        }
    }

    simplifiers![
        normalize::SNormalize,
        fold_multiplication::SFoldMultiplication,
        fold_addition::SFoldAddition,
        //
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
