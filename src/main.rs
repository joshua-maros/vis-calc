use std::{
    collections::HashMap,
    fmt::{Debug, Display},
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

macro_rules! e{
    ($name:ident($($args:expr),*)) => {
        Expression::Operator(stringify!($name).to_owned(), vec![$($args),*])
    };
    ($number:literal) => {
        Expression::Number($number.into())
    }
}

struct Operator {
    name: String,
    num_args: NumArgs,
    compute_rule: Option<ComputeRule>,
}

impl Operator {
    pub fn new(name: &str, num_args: NumArgs, compute_rule: Option<ComputeRule>) -> Self {
        Self {
            name: name.to_owned(),
            num_args,
            compute_rule,
        }
    }

    pub fn truee() -> Self {
        Self::new("true", 0.into(), None)
    }

    pub fn falsee() -> Self {
        Self::new("false", 0.into(), None)
    }

    pub fn and() -> Self {
        Self::new("and", 2.into(), None)
    }

    pub fn or() -> Self {
        Self::new("or", 2.into(), None)
    }

    pub fn not() -> Self {
        Self::new("not", 1.into(), None)
    }

    pub fn implies() -> Self {
        Self::new("imp", 2.into(), None)
    }

    pub fn biconditional() -> Self {
        Self::new("bicond", 2.into(), None)
    }

    pub fn equal() -> Self {
        fn cr(args: &[Expression]) -> Option<Expression> {
            if let (Expression::Number(l), Expression::Number(r)) = (&args[0], &args[1]) {
                Some(if l == r { e!(true()) } else { e!(false()) })
            } else {
                None
            }
        }

        Self::new("eq", 2.into(), Some(cr))
    }

    pub fn not_equal() -> Self {
        Self::new("neq", 2.into(), None)
    }

    pub fn add() -> Self {
        fn cr(args: &[Expression]) -> Option<Expression> {
            if let (Expression::Number(l), Expression::Number(r)) = (&args[0], &args[1]) {
                Some(Expression::Number(*l + *r))
            } else {
                None
            }
        }

        Self::new("add", 2.into(), Some(cr))
    }

    pub fn sub() -> Self {
        fn cr(args: &[Expression]) -> Option<Expression> {
            if let (Expression::Number(l), Expression::Number(r)) = (&args[0], &args[1]) {
                Some(Expression::Number(*l - *r))
            } else {
                None
            }
        }

        Self::new("sub", 2.into(), Some(cr))
    }

    pub fn neg() -> Self {
        fn cr(args: &[Expression]) -> Option<Expression> {
            if let &Expression::Number(r) = &args[0] {
                Some(Expression::Number(-r))
            } else {
                None
            }
        }

        Self::new("neg", 1.into(), Some(cr))
    }

    pub fn mul() -> Self {
        fn cr(args: &[Expression]) -> Option<Expression> {
            if let (Expression::Number(l), Expression::Number(r)) = (&args[0], &args[1]) {
                Some(Expression::Number(*l * *r))
            } else {
                None
            }
        }

        Self::new("mul", 2.into(), Some(cr))
    }

    pub fn div() -> Self {
        fn cr(args: &[Expression]) -> Option<Expression> {
            if let (Expression::Number(l), Expression::Number(r)) = (&args[0], &args[1]) {
                Some(Expression::Number(*l / *r))
            } else {
                None
            }
        }

        Self::new("div", 2.into(), Some(cr))
    }

    pub fn modd() -> Self {
        fn cr(args: &[Expression]) -> Option<Expression> {
            if let (Expression::Number(l), Expression::Number(r)) = (&args[0], &args[1]) {
                Some(Expression::Number(*l % *r))
            } else {
                None
            }
        }

        Self::new("mod", 2.into(), Some(cr))
    }
}

enum Expression {
    Number(Number),
    Operator(String, Vec<Expression>),
    Variable(String),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(num) => write!(f, "{}", num),
            Self::Operator(name, args) => {
                write!(f, "{}(", name)?;
                let mut first = true;
                for arg in args {
                    if first {
                        first = false
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Self::Variable(name) => write!(f, "{}", name),
        }
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
}

struct Environment {
    pub operators: HashMap<String, Operator>,
}

impl Environment {
    pub fn new() -> Self {
        let mut this = Environment {
            operators: HashMap::new(),
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
}

fn main() {
    let env = Environment::new();
    let expr = e!(add(e!(1), e!(1)));
    println!("{}", expr);
    println!("{}", expr.apply_computation(&env));
}
