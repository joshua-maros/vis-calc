use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    time::Instant,
};

use indexmap::IndexMap;
use itertools::Itertools;

use crate::{
    expression::Expression, make_expr, matchh::MatchResult, number::Number, simplify::Simplifier,
};

#[derive(Debug)]
pub struct SFoldMultiplication;

impl Simplifier for SFoldMultiplication {
    fn apply(&self, to: &mut Expression) {
        if let Expression::Operator(name, args) = to {
            if name != "mul" {
                return;
            }
            let old_args = std::mem::take(args);
            // Detect zeros.
            for arg in &old_args {
                if arg == &Expression::Number(0.into()) {
                    *to = Expression::Number(0.into());
                    return;
                }
            }
            // Remove ones and record exponents of all factors.
            let mut exponents = IndexMap::<Expression, Expression>::new();
            for arg in old_args {
                if arg == Expression::Number(1.into()) {
                    continue;
                } else {
                    let (base, exponent) = match arg {
                        Expression::Operator(name, args) if name == "pow" => {
                            args.into_iter().collect_tuple().unwrap()
                        }
                        other => (other, Expression::Number(1.into())),
                    };
                    if let Some(existing) = exponents.get_mut(&base) {
                        *existing =
                            Expression::Operator(format!("add"), vec![existing.clone(), exponent]);
                    } else {
                        exponents.insert(base, exponent);
                    }
                }
            }

            for (base, exponent) in exponents {
                if exponent == Expression::Number(1.into()) {
                    args.push(base);
                } else {
                    args.push(Expression::Operator(format!("pow"), vec![base, exponent]));
                }
            }

            if args.len() == 1 {
                let (arg,) = std::mem::take(args).into_iter().collect_tuple().unwrap();
                *to = arg;
            }
        }
    }
}
