use std::{collections::HashSet, fmt::Debug, time::Instant};

use itertools::Itertools;
use matchh::MatchResult;
use number::Number;

use crate::expression::Expression;

mod expression;
mod matchh;
mod number;

#[derive(Debug)]
struct Environment {
    pub simplifiers: Vec<Box<dyn Simplifier>>,
}

impl Environment {
    pub fn new() -> Self {
        let mut this = Environment {
            simplifiers: Vec::new(),
        };

        this.add_simplifier(SNormalize);
        this.add_simplifier(SFoldAddition);

        // this.add_simplifier(SAddIdentity);
        // this.add_simplifier(SAddIdentical);
        // this.add_simplifier(SAddFactorToMulFactor);
        // this.add_simplifier(SAddXyXz);

        // this.add_simplifier(SMulZero);
        // this.add_simplifier(SMulIdentity);
        // this.add_simplifier(SMulIdentical);
        // this.add_simplifier(SMulFactorWithPowFactor);
        // this.add_simplifier(SMulXyXz);

        // this.add_simplifier(SdAddRule);
        // this.add_simplifier(SdConstant);
        // this.add_simplifier(SdDivRule);
        // this.add_simplifier(SdIdentical);
        // this.add_simplifier(SdMulRule);
        // this.add_simplifier(SdSubRule);

        this.add_simplifier(ScAdd);
        this.add_simplifier(ScSub);
        this.add_simplifier(ScNeg);
        this.add_simplifier(ScMul);
        this.add_simplifier(ScDiv);
        this.add_simplifier(ScPow);

        this
    }

    pub fn add_simplifier(&mut self, simp: impl Simplifier + 'static) {
        self.simplifiers.push(Box::new(simp));
    }

    pub fn simplify(&self, expr: &mut Expression) {
        let start = Instant::now();
        while self.simplify_once(expr) {}
        println!("Took {}us", (Instant::now() - start).as_micros());
    }

    fn simplify_once(&self, expr: &mut Expression) -> bool {
        let mut simplified = false;
        match expr {
            Expression::Operator(_, args) => {
                for arg in args {
                    simplified |= self.simplify_once(arg);
                }
            }
            _ => (),
        }
        for simplifier in &self.simplifiers {
            simplified |= simplifier.apply(expr);
        }
        simplified
    }
}

trait Simplifier: Debug {
    fn apply(&self, to: &mut Expression) -> bool;
}

macro_rules! equality_simplifier {
    ($Name:ident = $($sources:tt),* => $replacement:tt) => {
        #[derive(Debug)]
        struct $Name;
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

#[derive(Debug)]
struct SNormalize;

fn collapse_op(name: &str, args: &mut Vec<Expression>) -> bool {
    let mut changed = false;
    let old_args = std::mem::take(args);
    for mut arg in old_args {
        if let Expression::Operator(arg_op, arg_args) = &mut arg {
            if arg_op == name {
                changed = true;
                args.append(arg_args);
                continue;
            }
        }
        args.push(arg);
    }
    changed
}

fn convert_op(
    name: &mut String,
    new_name: &str,
    args: &mut Vec<Expression>,
    map_last: impl Fn(Expression) -> Expression,
) -> bool {
    *name = String::from(new_name);
    let last = args.pop().unwrap();
    let last = map_last(last);
    args.push(last);
    true
}

impl Simplifier for SNormalize {
    fn apply(&self, to: &mut Expression) -> bool {
        let neg1 = <i32 as Into<Number>>::into(-1i32);
        if let Expression::Operator(name, args) = to {
            match &name[..] {
                "add" | "mul" | "pow" => collapse_op(&name[..], args),
                "sub" => convert_op(name, "add", args, |e| {
                    Expression::Operator(format!("mul"), vec![Expression::Number(neg1), e])
                }),
                "div" => convert_op(name, "mul", args, |e| {
                    Expression::Operator(format!("pow"), vec![e, Expression::Number(neg1)])
                }),
                "sqrt" => {
                    *name = format!("pow");
                    args.push(Expression::Number(0.5.into()));
                    true
                }
                _ => false,
            }
        } else {
            false
        }
    }
}

#[derive(Debug)]
struct SFoldAddition;

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
    if let Some(replacement) = eliminate_common_factors(args) {
        Some((replacement, vec![]))
    } else if args.len() > 2 {
        for index in 0..args.len() {
            let fewer_args = args
                .iter()
                .enumerate()
                .filter(|(idx, _)| *idx != index)
                .map(|(_, item)| item)
                .cloned()
                .collect_vec();
            if let Some((replacement, mut sum)) = aggressively_eliminate_common_factors(&fewer_args)
            {
                sum.push(args[index].clone());
                return Some((replacement, sum));
            }
        }
        None
    } else {
        None
    }
}

impl Simplifier for SFoldAddition {
    fn apply(&self, to: &mut Expression) -> bool {
        if let Expression::Operator(name, args) = to {
            if name != "add" {
                return false;
            }
            let old_args = std::mem::take(args);
            let mut changed = false;
            // Remove zeros.
            for arg in old_args {
                if arg == Expression::Number(0.into()) {
                    changed = true;
                } else {
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
                changed = true;
            }
            changed
        } else {
            false
        }
    }
}

equality_simplifier!(
    SAddIdentity =
    (add x 0),
    (add 0 x)
    => x
);

equality_simplifier!(
    SAddIdentical =
    (add x x)
    => (mul 2 x)
);

equality_simplifier!(
    SAddFactorToMulFactor =
    (add x (mul y x)),
    (add (mul y x) x),
    (add x (mul x y)),
    (add (mul x y) x)
    => (mul (add y 1) x)
);

equality_simplifier!(
    SAddXyXz=
    (add (mul x y) (mul x z)),
    (add (mul y x) (mul x z)),
    (add (mul x y) (mul z x)),
    (add (mul y x) (mul z x))
    => (mul (add y z) x)
);

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

macro_rules! computation_simplifier {
    ($Name:ident, $op:literal, $args:ident, $result:expr) => {
        #[derive(Debug)]
        struct $Name;

        impl Simplifier for $Name {
            fn apply(&self, to: &mut Expression) -> bool {
                if let Expression::Operator(name, args) = to {
                    if name == $op {
                        if let Some($args) = args
                            .iter()
                            .map(|x| x.as_number().copied())
                            .collect::<Option<Vec<_>>>()
                        {
                            let new_num = $result;
                            *to = Expression::Number(new_num);
                            return true;
                        }
                    }
                }
                false
            }
        }
    };
}

macro_rules! vararg_computation_simplifier {
    ($Name:ident, $op:literal, $a:ident, $b:ident, $result:expr) => {
        #[derive(Debug)]
        struct $Name;

        impl Simplifier for $Name {
            fn apply(&self, to: &mut Expression) -> bool {
                if let Expression::Operator(name, args) = to {
                    if name == $op {
                        if let Some(args) = args
                            .iter()
                            .map(|x| x.as_number().copied())
                            .collect::<Option<Vec<_>>>()
                        {
                            let mut $a = args[0];
                            for &$b in &args[1..] {
                                $a = $result;
                            }
                            *to = Expression::Number($a);
                            return true;
                        }
                    }
                }
                false
            }
        }
    };
}

vararg_computation_simplifier!(ScAdd, "add", a, b, a + b);
vararg_computation_simplifier!(ScSub, "sub", a, b, a - b);
computation_simplifier!(ScNeg, "neg", args, -args[0]);
vararg_computation_simplifier!(ScMul, "mul", a, b, a * b);
vararg_computation_simplifier!(ScDiv, "div", a, b, a / b);
computation_simplifier!(ScPow, "pow", args, args[0].powf(args[1]));

fn main() {
    let env = Environment::new();
    println!("{:#?}", env);
    println!("Algebra engine initialized!");

    // let mut to_rewrite = make_expr!((add (add a a) (add a (mul a 2))));
    // let mut to_rewrite = make_expr!((mul (mul (add x y) (add x y)) (add x y)));
    // let mut to_rewrite = make_expr!((dif t (pow t 2)));
    let mut to_rewrite = make_expr!((add a a a b));
    env.simplify(&mut to_rewrite);
    println!("{:?}", to_rewrite);
}
