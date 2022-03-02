use std::{collections::HashSet, fmt::Debug, time::Instant};

use itertools::Itertools;

use crate::{
    expression::Expression, make_expr, matchh::MatchResult, number::Number, simplify::Simplifier,
};

#[derive(Debug)]
pub struct SNormalize;

fn collapse_op(name: &str, args: &mut Vec<Expression>) {
    let old_args = std::mem::take(args);
    for mut arg in old_args {
        if let Expression::Operator(arg_op, arg_args) = &mut arg {
            if arg_op == name {
                args.append(arg_args);
                continue;
            }
        }
        args.push(arg);
    }
}

fn convert_op(
    name: &mut String,
    new_name: &str,
    args: &mut Vec<Expression>,
    map_last: impl Fn(Expression) -> Expression,
) {
    *name = String::from(new_name);
    let last = args.pop().unwrap();
    let last = map_last(last);
    args.push(last);
}

impl Simplifier for SNormalize {
    fn apply(&self, to: &mut Expression) {
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
                }
                _ => (),
            }
        }
    }
}
