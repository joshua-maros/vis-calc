use std::{collections::HashSet, fmt::Debug};

use itertools::Itertools;

use crate::{expression::Expression, simplify::Simplifier};

#[derive(Debug)]
pub struct SFoldAddition;

fn factors(of: &Expression) -> Vec<Expression> {
    if let Expression::Operator(name, args) = of {
        if name == "mul" {
            return args.clone();
        }
    }
    vec![of.clone()]
}

fn eliminate_common_factors(args: &[Expression]) -> Option<Expression> {
    assert!(args.len() >= 2);
    let first_arg = args.first().unwrap();
    let mut gcf: HashSet<_> = factors(first_arg).into_iter().collect();
    let mut sum = vec![vec![]];
    for arg in &args[1..] {
        let factors = factors(arg);
        let factors_hs: HashSet<_> = factors.into_iter().collect();
        let discarded_from_gcf = gcf.difference(&factors_hs);
        for discarded in discarded_from_gcf {
            for addend in &mut sum {
                addend.push(discarded.clone());
            }
        }
        gcf = gcf.intersection(&factors_hs).cloned().collect();
        let addend = factors_hs.difference(&gcf).cloned().collect_vec();
        sum.push(addend);
    }
    gcf = gcf
        .into_iter()
        .filter(|factor| factor != &Expression::Number(1.into()))
        .collect();
    if gcf.len() == 0 {
        None
    } else {
        let mut args = gcf.into_iter().collect_vec();
        let sum = sum
            .into_iter()
            .map(|addend| {
                if addend.len() == 0 {
                    Expression::Number(1.into())
                } else if addend.len() == 1 {
                    addend.into_iter().next().unwrap()
                } else {
                    Expression::Operator(format!("mul"), addend)
                }
            })
            .collect();
        args.push(Expression::Operator(format!("add"), sum));
        Some(Expression::Operator(format!("mul"), args))
    }
}

fn aggressively_eliminate_common_factors(
    args: &[Expression],
) -> Option<(Expression, Vec<Expression>)> {
    type Candidate = (Vec<Expression>, Vec<Expression>);
    let mut current_candidates: Vec<Candidate> = vec![(Vec::from(args), vec![])];
    let mut next_candidates: Vec<Candidate> = vec![];
    while current_candidates.len() > 0 {
        for candidate in current_candidates {
            if let Some(replacement) = eliminate_common_factors(&candidate.0) {
                return Some((replacement, candidate.1));
            }
            if candidate.0.len() > 2 {
                for omit_index in 0..candidate.0.len() {
                    let mut omitted_prod = candidate.0.clone();
                    let mut new_sum = candidate.1.clone();
                    new_sum.push(omitted_prod.remove(omit_index));
                    next_candidates.push((omitted_prod, new_sum));
                }
            }
        }
        current_candidates = next_candidates;
        next_candidates = Vec::new();
    }
    None
}

impl Simplifier for SFoldAddition {
    fn apply(&self, to: &mut Expression) {
        if let Expression::Operator(name, args) = to {
            if name != "add" {
                return;
            }
            let old_args = std::mem::take(args);
            // Remove zeros.
            for arg in old_args {
                if arg != Expression::Number(0.into()) {
                    args.push(arg);
                }
            }
            if let Some((replacement, sum)) = aggressively_eliminate_common_factors(&args) {
                if sum.len() == 0 {
                    *to = replacement;
                } else {
                    let mut args = sum;
                    args.push(replacement);
                    *to = Expression::Operator(format!("add"), args);
                }
            }
        }
    }
}
