use self::polynomial_expansion::SPolynomialExpansion;
use super::Simplifiers;
use crate::simplifiers;

/// A superset of "the distributive property", "FOIL", "binomial expansion",
/// etc.
mod polynomial_expansion {
    use itertools::Itertools;

    use crate::{
        expression::Expression,
        simplify::{SNormalize, Simplifier},
    };

    fn find_addends(factor: &Expression) -> Vec<&Expression> {
        match factor {
            Expression::Operator(name, args) if name == "add" => args.iter().collect(),
            other => vec![other],
        }
    }

    fn make_terms(sums: &[Vec<&Expression>]) -> Vec<Expression> {
        if sums.len() == 1 {
            sums[0].iter().copied().cloned().collect()
        } else {
            let combine_with = &sums[0];
            make_terms(&sums[1..])
                .into_iter()
                .flat_map(|expr| {
                    combine_with.iter().map(move |other| {
                        Expression::Operator(format!("mul"), vec![(*other).clone(), expr.clone()])
                    })
                })
                .collect()
        }
    }

    fn find_expansion(factors: &[&Expression]) -> Expression {
        let sums = factors.iter().copied().map(find_addends).collect_vec();
        Expression::Operator(format!("add"), make_terms(&sums[..]))
    }

    #[derive(Debug)]
    pub struct SPolynomialExpansion;

    impl Simplifier for SPolynomialExpansion {
        fn apply(&self, to: &mut Expression) {
            match to {
                Expression::Operator(name, args) if name == "mul" => {
                    let mut total_resulting_terms = 1;
                    for arg in &*args {
                        total_resulting_terms *= find_addends(arg).len();
                    }
                    if total_resulting_terms == 1 {
                        return;
                    }
                    let args = args.iter().collect_vec();
                    *to = find_expansion(&args[..]);
                    SNormalize.apply(to);
                }
                _ => (),
            }
        }
    }
}

pub fn simplifiers() -> Simplifiers {
    simplifiers![SPolynomialExpansion,]
}
