use std::fmt::Debug;

use itertools::Itertools;

use crate::{expression::Expression, simplify::Simplifier};

macro_rules! computation_simplifier {
    ($Name:ident, $op:literal, $args:ident, $result:expr) => {
        #[derive(Debug)]
        pub struct $Name;

        impl Simplifier for $Name {
            fn apply(&self, to: &mut Expression) {
                if let Expression::Operator(name, args) = to {
                    if name != $op {
                        return;
                    }
                    if let Some($args) = args
                        .iter()
                        .map(|x| x.as_number().copied())
                        .collect::<Option<Vec<_>>>()
                    {
                        let new_num = $result;
                        *to = Expression::Number(new_num);
                    }
                }
            }
        }
    };
}

// Here "movable" means that for all a, b, c, (a * b) * c = (a * c) * b.
// If the operation has a left identity, it implies communativity and
// associativity. But - is an operator without either of those but still follows
// the above property (which is allowed because it has no left identity.)
// https://math.stackexchange.com/questions/455960/when-i-state-a-cdot-b-cdot-c-a-cdot-c-cdot-b-what-properties-am-i-u
macro_rules! movable_vararg_computation_simplifier {
    ($Name:ident, $op:literal, $a:ident, $b:ident, $result:expr) => {
        #[derive(Debug)]
        pub struct $Name;

        impl Simplifier for $Name {
            fn apply(&self, to: &mut Expression) {
                if let Expression::Operator(name, args) = to {
                    if name != $op {
                        return;
                    }
                    let numeric_args = args
                        .iter()
                        .filter_map(|x| x.as_number().copied())
                        .collect_vec();
                    if numeric_args.len() <= 1 {
                        return;
                    }
                    let mut $a = numeric_args[0];
                    for &$b in &numeric_args[1..] {
                        $a = $result;
                    }
                    let mut new_args = args
                        .iter()
                        .cloned()
                        .filter(|x| !matches!(x, Expression::Number(..)))
                        .collect_vec();
                    if new_args.len() == 0 {
                        *to = Expression::Number($a);
                    } else {
                        new_args.push(Expression::Number($a));
                        *args = new_args;
                    }
                }
            }
        }
    };
}

movable_vararg_computation_simplifier!(ScAdd, "add", a, b, a + b);
computation_simplifier!(ScSub, "sub", args, args[0] - args[1]);
computation_simplifier!(ScNeg, "neg", args, -args[0]);
movable_vararg_computation_simplifier!(ScMul, "mul", a, b, a * b);
computation_simplifier!(ScDiv, "div", args, args[0] / args[1]);
computation_simplifier!(ScPow, "pow", args, args[0].powf(args[1]));
