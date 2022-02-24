use std::{
    collections::HashMap,
    fmt::Debug,
    hash::Hasher,
    ops::{Div, Mul},
};

use crate::{expression::Expression, operator::Operator};

mod expression;
mod matchh;
mod number;
mod operator;

#[derive(Debug)]
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
        this.add_operator(Operator::pow());

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
    println!("{:#?}", env);
    println!("Algebra engine initialized!");

    // let to_rewrite = expression!((and(and(true)(false))(true)));
    // let to_rewrite = expression!((add (add a a) (add a (mul a 2))));
    let _to_rewrite = make_expr!((mul (mul (add x y) (add x y)) (add x y)));
    // println!("{}", to_rewrite.flatten(&mut env));
    // let to_rewrite = expression!((add (add a a) (add a (mul a 2))));
    // env.simplify(to_rewrite, 4);
    // println!("{:?}", rewritten);
}
