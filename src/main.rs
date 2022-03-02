mod expression;
mod matchh;
mod number;
mod simplify;

use std::{fmt::Debug, time::Instant};

use simplify::{Simplifier, Simplifiers};

use crate::expression::Expression;

#[derive(Debug)]
struct Environment {
    passes: [Simplifiers; 5],
}

impl Environment {
    pub fn new() -> Self {
        let this = Environment {
            passes: [
                simplify::prepass(),
                simplify::intermediates(),
                simplify::expand(),
                simplify::intermediates(),
                simplify::collapse(),
            ],
        };
        this
    }

    pub fn simplify(&self, expr: &mut Expression) {
        let start = Instant::now();
        while self.simplify_once(expr) {}
        println!("Took {}us", (Instant::now() - start).as_micros());
    }

    fn simplify_once(&self, expr: &mut Expression) -> bool {
        let start = expr.clone();
        for pass in &self.passes {
            self.simplify_pass(expr, pass);
        }
        start != *expr
    }

    fn simplify_pass(&self, expr: &mut Expression, simps: &[Box<dyn Simplifier>]) {
        match expr {
            Expression::Operator(_, args) => {
                for arg in args {
                    self.simplify_once(arg);
                }
            }
            _ => (),
        }
        for simplifier in simps {
            simplifier.apply(expr);
        }
    }
}

fn main() {
    let env = Environment::new();
    println!("{:#?}", env);
    println!("Algebra engine initialized!");

    // let mut to_rewrite = make_expr!((add (add a a) (add a (mul a 2))));
    // let mut to_rewrite = make_expr!((mul (mul (add x y) (add x y)) (add x y)));
    // let mut to_rewrite = make_expr!((dif t (pow t 2)));
    // let mut to_rewrite = make_expr!((add a (mul a b)));
    // let mut to_rewrite = make_expr!((pow a 1));
    let mut to_rewrite = make_expr!((add (mul (add x 1) (add x 1)) (neg 1)));
    env.simplify(&mut to_rewrite);
    println!("{:?}", to_rewrite);
}
