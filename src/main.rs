use std::{fmt::Debug, time::Instant};

use matchh::MatchResult;

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

        this.add_simplifier(SAddIdentical);
        this.add_simplifier(SAddFactorToMulFactor);
        this.add_simplifier(SAddXyXz);
        this.add_simplifier(SMulIdentical);
        this.add_simplifier(SMulFactorWithPowFactor);
        this.add_simplifier(SMulXyXz);

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

computation_simplifier!(ScAdd, "add", args, args[0] + args[1]);
computation_simplifier!(ScSub, "sub", args, args[0] - args[1]);
computation_simplifier!(ScNeg, "neg", args, -args[0]);
computation_simplifier!(ScMul, "mul", args, args[0] * args[1]);
computation_simplifier!(ScDiv, "div", args, args[0] / args[1]);
computation_simplifier!(ScPow, "pow", args, args[0].powf(args[1]));

fn main() {
    let env = Environment::new();
    println!("{:#?}", env);
    println!("Algebra engine initialized!");

    // let to_rewrite = expression!((and(and(true)(false))(true)));
    // let mut to_rewrite = make_expr!((add (add a a) (add a (mul a 2))));
    let mut to_rewrite = make_expr!((mul (mul (add x y) (add x y)) (add x y)));
    // let to_rewrite = expression!((add (add a a) (add a (mul a 2))));
    env.simplify(&mut to_rewrite);
    println!("{:?}", to_rewrite);
}
