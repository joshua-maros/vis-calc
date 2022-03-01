use std::{collections::HashSet, fmt::Debug, time::Instant};

use itertools::Itertools;

use crate::{
    expression::Expression, make_expr, matchh::MatchResult, number::Number,
    simplify::Simplifier,
};

#[macro_export]
macro_rules! equality_simplifier {
    ($Name:ident = $($sources:tt),* => $replacement:tt) => {
        #[derive(Debug)]
        pub struct $Name;

        impl Simplifier for $Name {
            fn apply(&self, to: &mut Expression) -> bool {
                $(
                    if let MatchResult::Match(subs) = make_expr!($sources).matches_specific_case(&to) {
                        *to = make_expr!($replacement);
                        to.apply_substitutions(&subs);
                        return true;
                    }
                )*
                false
            }
        }
    }
}

equality_simplifier!(
    SMulZero =
    (mul x 0),
    (mul 0 x)
    => 0
);

equality_simplifier!(
    SMulIdentity =
    (mul x 1),
    (mul 1 x)
    => x
);

equality_simplifier!(
    SMulIdentical =
    (mul x x)
    => (pow x 2)
);

equality_simplifier!(
    SMulFactorWithPowFactor =
    (mul x (pow x y)),
    (mul (pow x y) x)
    => (pow x (add y 1))
);

equality_simplifier!(
    SMulXyXz=
    (mul (pow x y) (pow x z))
    => (mul x (add y z))
);
