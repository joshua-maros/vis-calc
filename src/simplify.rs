mod calculus;
mod computation;
mod equalities;
mod fold_addition;
mod normalize;

use std::{collections::HashSet, fmt::Debug, time::Instant};

use itertools::Itertools;

use crate::{expression::Expression, matchh::MatchResult, number::Number};

pub trait Simplifier: Debug {
    fn apply(&self, to: &mut Expression) -> bool;
}

pub fn simplifiers() -> Vec<Box<dyn Simplifier>> {
    macro_rules! simplifiers {
        ($($e:expr,)*) => {
            vec![$(Box::new($e)),*]
        }
    }

    simplifiers![
        calculus::SdAddRule,
        calculus::SdConstant,
        calculus::SdDivRule,
        calculus::SdIdentical,
        calculus::SdMulRule,
        calculus::SdSubRule,
        //
        computation::ScAdd,
        computation::ScDiv,
        computation::ScMul,
        computation::ScNeg,
        computation::ScPow,
        computation::ScSub,
        //
        equalities::SMulFactorWithPowFactor,
        equalities::SMulIdentical,
        equalities::SMulIdentity,
        equalities::SMulXyXz,
        equalities::SMulZero,
        //
        fold_addition::SFoldAddition,
        normalize::SNormalize,
    ]
}
