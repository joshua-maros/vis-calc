use std::{collections::HashSet, fmt::Debug, time::Instant};

use itertools::Itertools;

use crate::{
    equality_simplifier, expression::Expression, make_expr, matchh::MatchResult,
    number::Number, simplify::Simplifier,
};

equality_simplifier!(
    SdIdentical =
    (dif t t)
    => 1
);

#[derive(Debug)]
pub struct SdConstant;

impl Simplifier for SdConstant {
    fn apply(&self, to: &mut Expression) -> bool {
        if let MatchResult::Match(subs) = make_expr!((dif t a)).matches_specific_case(&to) {
            if let Expression::Number(..) = subs.get("a").unwrap() {
                *to = make_expr!(0);
                return true;
            }
        }
        false
    }
}

equality_simplifier!(
    SdAddRule =
    (dif t (add a b))
    => (add (dif t a) (dif t b))
);

equality_simplifier!(
    SdSubRule =
    (dif t (sub a b))
    => (sub (dif t a) (dif t b))
);

equality_simplifier!(
    SdMulRule =
    (dif t (mul a b))
    => (add (mul (dif t a) b) (mul (dif t b) a))
);

equality_simplifier!(
    SdDivRule =
    (dif t (div a b))
    => (div (sub (mul (dif t a) b) (mul (dif t b) a)) (pow b 2))
);
