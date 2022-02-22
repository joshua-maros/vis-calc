use std::{
    collections::HashMap,
    fmt::{Debug, Display, Formatter, self},
    ops::{Div, DivAssign, Mul, MulAssign, Rem, RemAssign},
};

use derive_more::*;

#[derive(Clone, Copy, PartialEq, PartialOrd, Neg, Add, AddAssign, Sub, SubAssign, Sum)]
struct Number(f64);

impl Mul for Number {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self {
        Self(self.0 * rhs.0)
    }
}

impl MulAssign for Number {
    fn mul_assign(&mut self, rhs: Self) {
        self.0 *= rhs.0
    }
}

impl Div for Number {
    type Output = Self;

    fn div(self, rhs: Self) -> Self {
        Self(self.0 / rhs.0)
    }
}

impl DivAssign for Number {
    fn div_assign(&mut self, rhs: Self) {
        self.0 /= rhs.0
    }
}

impl Rem for Number {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self {
        Self(self.0 % rhs.0)
    }
}

impl RemAssign for Number {
    fn rem_assign(&mut self, rhs: Self) {
        self.0 %= rhs.0
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<f64> for Number {
    fn from(value: f64) -> Self {
        Self(value)
    }
}

impl From<i32> for Number {
    fn from(value: i32) -> Self {
        Self(value as f64)
    }
}

#[derive(Debug)]
enum NumArgs {
    Variable,
    Fixed(usize),
}

use itertools::Itertools;
use NumArgs::Variable;

impl From<usize> for NumArgs {
    fn from(v: usize) -> Self {
        Self::Fixed(v)
    }
}

type ComputeRule = fn(&[Expression]) -> Option<Expression>;
type WeightRule = fn(&[usize]) -> usize;

macro_rules! expression{
    (($name:ident $($args:tt)*)) => {
        Expression::Operator(stringify!($name).to_owned(), vec![$(expression!($args)),*])
    };
    ($name:ident) => {
        Expression::Variable(stringify!($name).to_owned())
    };
    ($number:literal) => {
        Expression::Number($number.into())
    }
}

struct Operator {
    name: String,
    num_args: NumArgs,
    compute_rule: Option<ComputeRule>,
    weight_rule: WeightRule,
}

impl Debug for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Operator")
            .field("name", &self.name)
            .field("num_args", &self.num_args)
            .field("compute_rule", &self.compute_rule.is_some())
            .finish()
    }
}

fn basic_wr(operand_weights: &[usize]) -> usize {
    operand_weights.iter().copied().sum::<usize>() + 1
}

impl Operator {
    fn new_basic(name: &str, num_args: NumArgs, compute_rule: Option<ComputeRule>) -> Self {
        Self::new(name, num_args, compute_rule, basic_wr)
    }

    fn new(
        name: &str,
        num_args: NumArgs,
        compute_rule: Option<ComputeRule>,
        weight_rule: WeightRule,
    ) -> Self {
        Self {
            name: name.to_owned(),
            num_args,
            compute_rule,
            weight_rule,
        }
    }

    pub fn truee() -> Self {
        Self::new_basic("true", 0.into(), None)
    }

    pub fn falsee() -> Self {
        Self::new_basic("false", 0.into(), None)
    }

    pub fn and() -> Self {
        Self::new_basic("and", 2.into(), None)
    }

    pub fn or() -> Self {
        Self::new_basic("or", 2.into(), None)
    }

    pub fn not() -> Self {
        Self::new_basic("not", 1.into(), None)
    }

    pub fn implies() -> Self {
        Self::new_basic("imp", 2.into(), None)
    }

    pub fn biconditional() -> Self {
        Self::new_basic("bicond", 2.into(), None)
    }

    pub fn equal() -> Self {
        fn cr(args: &[Expression]) -> Option<Expression> {
            if args.len() == 0 {
                Some(expression!((true)))
            } else {
                let start = *args[0].as_number()?;
                let mut result = true;
                for other in &args[1..] {
                    result = result && start == *other.as_number()?;
                }
                Some(if result {
                    expression!((true))
                } else {
                    expression!((false))
                })
            }
        }

        Self::new_basic("eq", Variable, Some(cr))
    }

    pub fn not_equal() -> Self {
        Self::new_basic("neq", 2.into(), None)
    }

    pub fn add() -> Self {
        fn cr(args: &[Expression]) -> Option<Expression> {
            if let (Expression::Number(l), Expression::Number(r)) = (&args[0], &args[1]) {
                Some(Expression::Number(*l + *r))
            } else {
                None
            }
        }

        Self::new_basic("add", 2.into(), Some(cr))
    }

    pub fn sub() -> Self {
        fn cr(args: &[Expression]) -> Option<Expression> {
            if let (Expression::Number(l), Expression::Number(r)) = (&args[0], &args[1]) {
                Some(Expression::Number(*l - *r))
            } else {
                None
            }
        }

        Self::new_basic("sub", 2.into(), Some(cr))
    }

    pub fn neg() -> Self {
        fn cr(args: &[Expression]) -> Option<Expression> {
            if let &Expression::Number(r) = &args[0] {
                Some(Expression::Number(-r))
            } else {
                None
            }
        }

        Self::new_basic("neg", 1.into(), Some(cr))
    }

    pub fn mul() -> Self {
        fn cr(args: &[Expression]) -> Option<Expression> {
            if let (Expression::Number(l), Expression::Number(r)) = (&args[0], &args[1]) {
                Some(Expression::Number(*l * *r))
            } else {
                None
            }
        }

        Self::new_basic("mul", 2.into(), Some(cr))
    }

    pub fn div() -> Self {
        fn cr(args: &[Expression]) -> Option<Expression> {
            if let (Expression::Number(l), Expression::Number(r)) = (&args[0], &args[1]) {
                Some(Expression::Number(*l / *r))
            } else {
                None
            }
        }

        Self::new_basic("div", 2.into(), Some(cr))
    }

    pub fn modd() -> Self {
        fn cr(args: &[Expression]) -> Option<Expression> {
            if let (Expression::Number(l), Expression::Number(r)) = (&args[0], &args[1]) {
                Some(Expression::Number(*l % *r))
            } else {
                None
            }
        }

        Self::new_basic("mod", 2.into(), Some(cr))
    }
}

#[derive(Clone, PartialEq)]
enum Expression {
    Number(Number),
    Operator(String, Vec<Expression>),
    Variable(String),
}

impl Debug for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        <Self as Display>::fmt(&self, f)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(num) => write!(f, "{}", num),
            Self::Operator(name, args) => {
                write!(f, "({}", name)?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
                write!(f, ")")
            }
            Self::Variable(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum MatchResult {
    Match {
        substitutions: HashMap<String, Expression>,
    },
    NoMatch,
}

impl MatchResult {
    pub fn empty_match() -> Self {
        Self::Match {
            substitutions: HashMap::new(),
        }
    }

    pub fn match_if(condition: bool) -> Self {
        match condition {
            true => Self::empty_match(),
            false => Self::NoMatch,
        }
    }

    pub fn and(l: Self, r: Self) -> Self {
        if let (
            Self::Match {
                substitutions: lsubs,
            },
            Self::Match {
                substitutions: rsubs,
            },
        ) = (l, r)
        {
            let mut output_subs = HashMap::new();
            for (target, value) in lsubs.clone() {
                if rsubs.contains_key(&target) {
                    todo!()
                } else {
                    output_subs.insert(target, value);
                }
            }
            for (target, value) in rsubs {
                if !lsubs.contains_key(&target) {
                    output_subs.insert(target, value);
                }
            }
            Self::Match {
                substitutions: output_subs,
            }
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

impl Expression {
    fn apply_computation(self, env: &Environment) -> Self {
        match self {
            Self::Operator(name, args) => {
                let args = args
                    .into_iter()
                    .map(|arg| arg.apply_computation(env))
                    .collect_vec();
                if let Some(cr) = env.get_operator(&name).compute_rule {
                    if let Some(computed) = cr(&args) {
                        return computed;
                    }
                }
                Self::Operator(name, args)
            }
            other => other,
        }
    }

    fn as_number(&self) -> Option<&Number> {
        if let Self::Number(v) = self {
            Some(v)
        } else {
            None
        }
    }

    fn matches_specific_case(&self, specific_case: &Self) -> MatchResult {
        match (self, specific_case) {
            (Expression::Number(l), Expression::Number(r)) => MatchResult::match_if(l == r),
            (Expression::Operator(lname, largs), Expression::Operator(rname, rargs)) => {
                let mut result = MatchResult::match_if(lname == rname && largs.len() == rargs.len());
                for (larg, rarg) in largs.iter().zip(rargs.iter()) {
                    result = MatchResult::and(result, larg.matches_specific_case(rarg));
                }
                result
            },
            (Expression::Variable(l), Expression::Variable(r)) => MatchResult::match_if(l == r),
            _ => MatchResult::NoMatch
        }
    }
}

struct RewriteRule {
    preconditions: Vec<Expression>,
    original: Expression,
    rewritten: Expression,
}

impl Debug for RewriteRule {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut first = true;
        for pre in &self.preconditions {
            if first {
                first = false
            } else {
                write!(f, " ^ ")?;
            }
            write!(f, "{}", pre)?;
        }
        if self.preconditions.len() > 0 {
            write!(f, " -> ")?;
        }
        write!(f, "{} = {}", self.original, self.rewritten)
    }
}

impl TryFrom<Expression> for RewriteRule {
    type Error = ();

    fn try_from(mut value: Expression) -> Result<Self, ()> {
        let mut preconditions = Vec::new();
        while let Expression::Operator(name, args) = &value {
            if name == "imp" {
                preconditions.push(args[0].clone());
                value = args[1].clone();
            } else {
                break;
            }
        }
        if let Expression::Operator(name, mut args) = value {
            if name == "eq" && args.len() == 2 {
                return Ok(Self {
                    preconditions,
                    rewritten: args.pop().unwrap(),
                    original: args.pop().unwrap(),
                });
            }
        }
        Err(())
    }
}

#[derive(Debug)]
struct Environment {
    pub operators: HashMap<String, Operator>,
    pub rewrite_rules: Vec<RewriteRule>,
}

impl Environment {
    pub fn new() -> Self {
        let mut this = Environment {
            operators: HashMap::new(),
            rewrite_rules: Vec::new(),
        };

        this.add_operator(Operator::truee());
        this.add_operator(Operator::falsee());
        this.add_operator(Operator::and());
        this.add_operator(Operator::or());
        this.add_operator(Operator::not());
        this.add_operator(Operator::implies());
        this.add_operator(Operator::biconditional());
        this.add_operator(Operator::equal());
        this.add_operator(Operator::not_equal());
        this.add_operator(Operator::add());
        this.add_operator(Operator::sub());
        this.add_operator(Operator::neg());
        this.add_operator(Operator::mul());
        this.add_operator(Operator::div());
        this.add_operator(Operator::modd());

        this
    }

    pub fn add_operator(&mut self, op: Operator) {
        self.operators.insert(op.name.clone(), op);
    }

    pub fn get_operator(&self, name: &str) -> &Operator {
        self.operators.get(name).unwrap()
    }

    pub fn add_rewrite_rule(&mut self, from: Expression) {
        self.rewrite_rules.push(from.try_into().unwrap())
    }
}

macro_rules! truth_table {
    ($env:expr, $operator:ident $tt:ident $tf:ident $ft:ident $ff:ident) => {
        $env.add_rewrite_rule(expression!((eq ($operator (true) (true)) ($tt))));
        $env.add_rewrite_rule(expression!((eq ($operator (true) (false)) ($tf))));
        $env.add_rewrite_rule(expression!((eq ($operator (false) (true)) ($ft))));
        $env.add_rewrite_rule(expression!((eq ($operator (false) (false)) ($ff))));
    }
}

fn main() {
    let mut env = Environment::new();
    truth_table!(env, and true false false false);
    truth_table!(env, or true true true false);
    truth_table!(env, imp true false true true);
    truth_table!(env, bicond true false false true);
    env.add_rewrite_rule(expression!((eq (not (true)) (false))));
    env.add_rewrite_rule(expression!((eq (not (false)) (true))));
    env.add_rewrite_rule(expression!((eq (add a b) (add b a))));
    env.add_rewrite_rule(expression!((eq (sub 0 a) (neg a))));
    env.add_rewrite_rule(expression!((eq (add (add a b) c) (add a (add b c)))));
    println!("{:#?}", env);
}
