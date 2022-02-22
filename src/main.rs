use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

use derive_more::*;

#[derive(
    Clone,
    Copy,
    PartialEq,
    PartialOrd,
    Neg,
    Add,
    AddAssign,
    Sub,
    SubAssign,
    Mul,
    MulAssign,
    Div,
    DivAssign,
    Rem,
    RemAssign,
    Sum,
)]
struct Number(f64);

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
    let expr = e!(eq(e!(1), e!(1)));
    println!("{}", expr);
    println!("{}", expr.apply_computation(&env));
}
