use std::collections::HashMap;

use maplit::hashmap;

use crate::expression::Expression;

pub type Substitutions = HashMap<String, Expression>;

#[derive(Clone, Debug, PartialEq)]
pub enum MatchResult {
    Match(Substitutions),
    NoMatch,
}

impl MatchResult {
    pub fn empty_match() -> Self {
        Self::Match(Substitutions::new())
    }

    pub fn match_if(condition: bool) -> Self {
        match condition {
            true => Self::empty_match(),
            false => Self::NoMatch,
        }
    }

    pub fn and(l: Self, r: Self) -> Self {
        if let (Self::Match(lsubs), Self::Match(rsubs)) = (l, r) {
            let mut output_subs = Substitutions::new();
            for (target, value) in lsubs.clone() {
                if let Some(rsub) = rsubs.get(&target) {
                    if rsub != &value {
                        return Self::NoMatch;
                    }
                }
                output_subs.insert(target, value);
            }
            for (target, value) in rsubs {
                if !lsubs.contains_key(&target) {
                    output_subs.insert(target, value);
                }
            }
            Self::Match(output_subs)
        } else {
            Self::NoMatch
        }
    }

    /// Returns `true` if the match result is [`NoMatch`].
    ///
    /// [`NoMatch`]: MatchResult::NoMatch
    fn is_no_match(&self) -> bool {
        matches!(self, Self::NoMatch)
    }
}

/// First arguments to follow are at the end. E.G. a path of 1, 0 in the
/// expression (a (b c d) (e f g)) gets you to "d".
type ExpressionPath = Vec<usize>;

impl Expression {
    fn matches_specific_case(&self, specific_case: &Self) -> MatchResult {
        match (self, specific_case) {
            (Expression::Number(l), Expression::Number(r)) => MatchResult::match_if(l == r),
            (Expression::Operator(lname, largs), Expression::Operator(rname, rargs)) => {
                let mut result =
                    MatchResult::match_if(lname == rname && largs.len() == rargs.len());
                for (larg, rarg) in largs.iter().zip(rargs.iter()) {
                    let args_match = larg.matches_specific_case(rarg);
                    result = MatchResult::and(result, args_match);
                }
                // if !result.is_no_match() {
                //     println!("{} {}, {:?}", self, specific_case, result);
                // }
                result
            }
            (Expression::Variable(l), _) => {
                MatchResult::Match(hashmap![l.clone() => specific_case.clone()])
            }
            _ => MatchResult::NoMatch,
        }
    }

    fn find_all_matches_in(&self, specific_case: &Self) -> Vec<(Substitutions, ExpressionPath)> {
        let result = if let MatchResult::Match(subs) = self.matches_specific_case(specific_case) {
            Some((subs, vec![]))
        } else {
            None
        };
        if let Self::Operator(_, args) = specific_case {
            args.iter()
                .enumerate()
                .flat_map(|(index, arg)| {
                    self.find_all_matches_in(arg)
                        .into_iter()
                        .map(move |mut result| {
                            result.1.push(index);
                            result
                        })
                })
                .chain(result.into_iter())
                .collect()
        } else {
            result.into_iter().collect()
        }
    }

    fn apply_substitutions(&mut self, subs: &Substitutions) {
        match self {
            Expression::Operator(_, args) => {
                for arg in args {
                    arg.apply_substitutions(subs);
                }
            }
            Expression::Variable(name) => {
                if let Some(replacement) = subs.get(name) {
                    *self = replacement.clone()
                }
            }
            _ => (),
        }
    }
}
