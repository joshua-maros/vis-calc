mod calculus;
mod computation;
mod equalities;

use super::Simplifiers;
use crate::simplifiers;

pub fn simplifiers() -> Simplifiers {
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
