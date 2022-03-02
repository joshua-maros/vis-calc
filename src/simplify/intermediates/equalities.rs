use std::fmt::Debug;

use crate::{expression::Expression, make_expr, matchh::MatchResult, simplify::Simplifier};

#[macro_export]
macro_rules! equality_simplifier {
    ($Name:ident = $($sources:tt),* => $replacement:tt) => {
        #[derive(Debug)]
        pub struct $Name;

        impl Simplifier for $Name {
            fn apply(&self, to: &mut Expression) {
                loop {
                $(
                    if let MatchResult::Match(subs) = make_expr!($sources).matches_specific_case(&to) {
                        println!("{:?}", make_expr!($sources));
                        *to = make_expr!($replacement);
                        to.apply_substitutions(&subs);
                        continue;
                    }
                )*
                    break;
                }
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
    SPowZero =
    (pow x 0)
    => 1
);

equality_simplifier!(
    SPowIdentity =
    (pow x 1)
    => x
);
