mod expression;
mod matchh;
mod number;
mod simplify;

use std::{fmt::Debug, time::Instant};

use simplify::Simplifier;

use crate::expression::Expression;

#[derive(Debug)]
struct Environment {
    pub simplifiers: Vec<Box<dyn Simplifier>>,
}

impl Environment {
    pub fn new() -> Self {
        let mut this = Environment {
            simplifiers: Vec::new(),
        };
        for simp in simplify::simplifiers() {
            this.simplifiers.push(simp);
        }
        this
    }

    pub fn simplify(&self, expr: &mut Expression) {
        let start = Instant::now();
        while self.simplify_once(expr) {}
        println!("Took {}us", (Instant::now() - start).as_micros());
    }

    fn simplify_once(&self, expr: &mut Expression) -> bool {
        let start = expr.clone();
        match expr {
            Expression::Operator(_, args) => {
                for arg in args {
                    self.simplify_once(arg);
                }
            }
            _ => (),
        }
        for simplifier in &self.simplifiers {
            simplifier.apply(expr);
        }
        start != *expr
    }
}

fn main() {
    let env = Environment::new();
    println!("{:#?}", env);
    println!("Algebra engine initialized!");

    // let mut to_rewrite = make_expr!((add (add a a) (add a (mul a 2))));
    // let mut to_rewrite = make_expr!((mul (mul (add x y) (add x y)) (add x y)));
    // let mut to_rewrite = make_expr!((dif t (pow t 2)));
    let mut to_rewrite = make_expr!((add a (mul a b)));
    // let mut to_rewrite = make_expr!((pow a 1));
    // let mut to_rewrite = make_expr!((mul 1 (pow a 1)));
    env.simplify(&mut to_rewrite);
    println!("{:?}", to_rewrite);
}
