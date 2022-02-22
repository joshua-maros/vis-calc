use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Debug, Display, Formatter},
    hash::{Hash, Hasher},
    ops::{Div, DivAssign, Mul, MulAssign, Rem, RemAssign},
    time::Instant,
};

use derive_more::*;
use itertools::Itertools;
use maplit::hashmap;

#[derive(Clone, Copy, PartialOrd, Neg, Add, AddAssign, Sub, SubAssign, Sum)]
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

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        self.0.to_le_bytes() == other.0.to_le_bytes()
    }
}

impl Eq for Number {}

impl Hash for Number {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write(&self.0.to_le_bytes());
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
    num_args: usize,
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
    fn new_basic(name: &str, num_args: usize, compute_rule: Option<ComputeRule>) -> Self {
        Self::new(name, num_args, compute_rule, basic_wr)
    }

    fn new(
        name: &str,
        num_args: usize,
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
        Self::new_basic("true", 0, None)
    }

    pub fn falsee() -> Self {
        Self::new_basic("false", 0, None)
    }

    pub fn and() -> Self {
        Self::new_basic("and", 2, None)
    }

    pub fn or() -> Self {
        Self::new_basic("or", 2, None)
    }

    pub fn not() -> Self {
        Self::new_basic("not", 1, None)
    }

    pub fn implies() -> Self {
        Self::new_basic("imp", 2, None)
    }

    pub fn biconditional() -> Self {
        Self::new_basic("bicond", 2, None)
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

        Self::new_basic("eq", 2, Some(cr))
    }

    pub fn not_equal() -> Self {
        Self::new_basic("neq", 2, None)
    }

    pub fn add() -> Self {
        fn cr(args: &[Expression]) -> Option<Expression> {
            if let (Expression::Number(l), Expression::Number(r)) = (&args[0], &args[1]) {
                Some(Expression::Number(*l + *r))
            } else {
                None
            }
        }

        Self::new_basic("add", 2, Some(cr))
    }

    pub fn sub() -> Self {
        fn cr(args: &[Expression]) -> Option<Expression> {
            if let (Expression::Number(l), Expression::Number(r)) = (&args[0], &args[1]) {
                Some(Expression::Number(*l - *r))
            } else {
                None
            }
        }

        Self::new_basic("sub", 2, Some(cr))
    }

    pub fn neg() -> Self {
        fn cr(args: &[Expression]) -> Option<Expression> {
            if let &Expression::Number(r) = &args[0] {
                Some(Expression::Number(-r))
            } else {
                None
            }
        }

        Self::new_basic("neg", 1, Some(cr))
    }

    pub fn mul() -> Self {
        fn cr(args: &[Expression]) -> Option<Expression> {
            if let (Expression::Number(l), Expression::Number(r)) = (&args[0], &args[1]) {
                Some(Expression::Number(*l * *r))
            } else {
                None
            }
        }

        Self::new_basic("mul", 2, Some(cr))
    }

    pub fn div() -> Self {
        fn cr(args: &[Expression]) -> Option<Expression> {
            if let (Expression::Number(l), Expression::Number(r)) = (&args[0], &args[1]) {
                Some(Expression::Number(*l / *r))
            } else {
                None
            }
        }

        Self::new_basic("div", 2, Some(cr))
    }

    pub fn modd() -> Self {
        fn cr(args: &[Expression]) -> Option<Expression> {
            if let (Expression::Number(l), Expression::Number(r)) = (&args[0], &args[1]) {
                Some(Expression::Number(*l % *r))
            } else {
                None
            }
        }

        Self::new_basic("mod", 2, Some(cr))
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
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

impl Expression {
    pub fn flatten(&self, env: &mut Environment) -> FlatExpression {
        let mut result = FlatExpression {
            arg_references: Vec::new(),
            components: Vec::new(),
        };
        self.flatten_into(&mut result, env);
        result
    }

    fn flatten_into(&self, into: &mut FlatExpression, env: &mut Environment) -> usize {
        let index = into.components.len();
        match self {
            Expression::Number(num) => into.components.push(FeComponent::Number(*num)),
            Expression::Variable(var) => into.components.push(FeComponent::Variable(var.clone())),
            Expression::Operator(name, args) => {
                let name = name.clone();
                let arg_start_index = into.arg_references.len();
                let num_args = args.len();
                for _ in 0..num_args {
                    into.arg_references.push(0);
                }
                into.components.push(FeComponent::Operator {
                    name,
                    arg_start_index,
                    num_args,
                });
                for index in 0..num_args {
                    into.arg_references[arg_start_index + index] =
                        args[index].flatten_into(into, env);
                }
            }
        }
        index
    }
}

enum FeComponent {
    Number(Number),
    Operator {
        name: String,
        arg_start_index: usize,
        num_args: usize,
    },
    Variable(String),
}

struct FlatExpression {
    arg_references: Vec<usize>,
    components: Vec<FeComponent>,
}

impl Display for FlatExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut paren_stack = Vec::new();
        for c in &self.components {
            paren_stack.last_mut().map(|l| *l -= 1);
            match c {
                FeComponent::Number(num) => write!(f, "{}", num)?,
                FeComponent::Operator { name, num_args, .. } => {
                    write!(f, "({}", name)?;
                    paren_stack.push(*num_args);
                }
                FeComponent::Variable(var) => write!(f, "{}", var)?,
            }
            while let Some(0) = paren_stack.last() {
                paren_stack.pop();
                write!(f, ")")?;
            }
            write!(f, " ")?;
        }
        Ok(())
    }
}

impl FlatExpression {
    fn replace(&mut self, node_index: usize, with: FlatExpression) {

    }

    #[must_use]
    fn apply_computation(self, env: &Environment) -> Self {
        // match self {
        //     Self::Operator(name, args) => {
        //         let args = args
        //             .into_iter()
        //             .map(|arg| arg.apply_computation(env))
        //             .collect_vec();
        //         if let Some(cr) = env.get_operator(&name).compute_rule {
        //             if let Some(computed) = cr(&args) {
        //                 return computed;
        //             }
        //         }
        //         Self::Operator(name, args)
        //     }
        //     other => other,
        // }
        todo!()
    }

    fn as_number(&self) -> Option<&Number> {
        // if let Self::Number(v) = self {
        //     Some(v)
        // } else {
        //     None
        // }
        todo!()
    }

    fn matches_specific_case(&self, specific_case: &Self) -> MatchResult {
        // match (self, specific_case) {
        //     (Expression::Number(l), Expression::Number(r)) => MatchResult::match_if(l == r),
        //     (Expression::Operator(lname, largs), Expression::Operator(rname, rargs)) => {
        //         let mut result =
        //             MatchResult::match_if(lname == rname && largs.len() == rargs.len());
        //         for (larg, rarg) in largs.iter().zip(rargs.iter()) {
        //             let args_match = larg.matches_specific_case(rarg);
        //             result = MatchResult::and(result, args_match);
        //         }
        //         result
        //     }
        //     (Expression::Variable(l), _) => {
        //         MatchResult::Match(hashmap![l.clone() => specific_case.clone()])
        //     }
        //     _ => MatchResult::NoMatch,
        // }
        todo!()
    }

    fn find_all_matches_in(&self, specific_case: &Self) -> Vec<(Substitutions, ExpressionPath)> {
        // let result = if let MatchResult::Match(subs) = self.matches_specific_case(specific_case) {
        //     Some((subs, vec![]))
        // } else {
        //     None
        // };
        // if let Self::Operator(_, args) = specific_case {
        //     args.iter()
        //         .enumerate()
        //         .flat_map(|(index, arg)| {
        //             self.find_all_matches_in(arg)
        //                 .into_iter()
        //                 .map(move |mut result| {
        //                     result.1.push(index);
        //                     result
        //                 })
        //         })
        //         .chain(result.into_iter())
        //         .collect()
        // } else {
        //     result.into_iter().collect()
        // }
        todo!()
    }

    fn apply_substitutions(&mut self, subs: &Substitutions) {
        // match self {
        //     Expression::Operator(_, args) => {
        //         for arg in args {
        //             arg.apply_substitutions(subs);
        //         }
        //     }
        //     Expression::Variable(name) => {
        //         if let Some(replacement) = subs.get(name) {
        //             *self = replacement.clone()
        //         }
        //     }
        //     _ => (),
        // }
        todo!()
    }

    fn apply_rewrite_rule(
        &mut self,
        rule: &RewriteRule,
        subs: &Substitutions,
        mut path: ExpressionPath,
    ) {
        // if let Some(index) = path.pop() {
        //     if let Self::Operator(_, args) = self {
        //         args[index].apply_rewrite_rule(rule, subs, path)
        //     } else {
        //         unreachable!()
        //     }
        // } else {
        //     *self = rule.rewritten.clone();
        //     self.apply_substitutions(&subs);
        // }
        todo!()
    }

    fn compute_weight(&self, env: &Environment) -> usize {
        // match self {
        //     Expression::Number(..) | Expression::Variable(..) => 1,
        //     Expression::Operator(name, args) => {
        //         let arg_weights = args.iter().map(|arg| arg.compute_weight(env)).collect_vec();
        //         (env.operators.get(name).unwrap().weight_rule)(&arg_weights)
        //     }
        // }
        todo!()
    }
}

type Substitutions = HashMap<String, Expression>;

#[derive(Clone, Debug, PartialEq)]
enum MatchResult {
    Match(Substitutions),
    NoMatch,
}

impl MatchResult {
    pub fn empty_match() -> Self {
        Self::Match(Substitutions::new())
    }

    pub fn match_if(condition: bool) -> Self {
        match condition {
            true => Self::empty_match(),
            false => Self::NoMatch,
        }
    }

    pub fn and(l: Self, r: Self) -> Self {
        if let (Self::Match(lsubs), Self::Match(rsubs)) = (l, r) {
            let mut output_subs = Substitutions::new();
            for (target, value) in lsubs.clone() {
                if let Some(rsub) = rsubs.get(&target) {
                    if rsub != &value {
                        return Self::NoMatch;
                    }
                } else {
                    output_subs.insert(target, value);
                }
            }
            for (target, value) in rsubs {
                if !lsubs.contains_key(&target) {
                    output_subs.insert(target, value);
                }
            }
            Self::Match(output_subs)
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

/// First arguments to follow are at the end. E.G. a path of 1, 0 in the
/// expression (a (b c d) (e f g)) gets you to "d".
type ExpressionPath = Vec<usize>;

impl Expression {
    #[must_use]
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
                let mut result =
                    MatchResult::match_if(lname == rname && largs.len() == rargs.len());
                for (larg, rarg) in largs.iter().zip(rargs.iter()) {
                    let args_match = larg.matches_specific_case(rarg);
                    result = MatchResult::and(result, args_match);
                }
                result
            }
            (Expression::Variable(l), _) => {
                MatchResult::Match(hashmap![l.clone() => specific_case.clone()])
            }
            _ => MatchResult::NoMatch,
        }
    }

    fn find_all_matches_in(&self, specific_case: &Self) -> Vec<(Substitutions, ExpressionPath)> {
        let result = if let MatchResult::Match(subs) = self.matches_specific_case(specific_case) {
            Some((subs, vec![]))
        } else {
            None
        };
        if let Self::Operator(_, args) = specific_case {
            args.iter()
                .enumerate()
                .flat_map(|(index, arg)| {
                    self.find_all_matches_in(arg)
                        .into_iter()
                        .map(move |mut result| {
                            result.1.push(index);
                            result
                        })
                })
                .chain(result.into_iter())
                .collect()
        } else {
            result.into_iter().collect()
        }
    }

    fn apply_substitutions(&mut self, subs: &Substitutions) {
        match self {
            Expression::Operator(_, args) => {
                for arg in args {
                    arg.apply_substitutions(subs);
                }
            }
            Expression::Variable(name) => {
                if let Some(replacement) = subs.get(name) {
                    *self = replacement.clone()
                }
            }
            _ => (),
        }
    }

    fn apply_rewrite_rule(
        &mut self,
        rule: &RewriteRule,
        subs: &Substitutions,
        mut path: ExpressionPath,
    ) {
        if let Some(index) = path.pop() {
            if let Self::Operator(_, args) = self {
                args[index].apply_rewrite_rule(rule, subs, path)
            } else {
                unreachable!()
            }
        } else {
            *self = rule.rewritten.clone();
            self.apply_substitutions(&subs);
        }
    }

    fn compute_weight(&self, env: &Environment) -> usize {
        match self {
            Expression::Number(..) | Expression::Variable(..) => 1,
            Expression::Operator(name, args) => {
                let arg_weights = args.iter().map(|arg| arg.compute_weight(env)).collect_vec();
                (env.operators.get(name).unwrap().weight_rule)(&arg_weights)
            }
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

    pub fn apply_all_applicable_rules(&self, to: &Expression) -> Vec<Expression> {
        let mut result = HashSet::new();
        for rule in &self.rewrite_rules {
            for (subs, match_path) in rule.original.find_all_matches_in(to) {
                let mut modified = to.clone();
                modified.apply_rewrite_rule(rule, &subs, match_path);
                result.insert(modified);
            }
        }
        result.into_iter().collect_vec()
    }

    pub fn simplify_limited(&self, src: Expression, limit: usize) -> Expression {
        let src = src.apply_computation(self);
        let mut best = src.clone();
        if limit > 0 {
            for alternate_form in self.apply_all_applicable_rules(&src) {
                let simplified = self.simplify_limited(alternate_form, limit - 1);
                if simplified.compute_weight(self) < best.compute_weight(self) {
                    best = simplified;
                }
            }
        }
        best
    }

    pub fn simplify(&self, src: Expression, strength: usize) -> Expression {
        let mut result = src;
        'outer: loop {
            let start = Instant::now();
            for limit in 0..strength {
                let old_weight = result.compute_weight(self);
                result = self.simplify_limited(result, limit);
                if result.compute_weight(self) < old_weight {
                    println!("{} {}", (Instant::now() - start).as_micros(), result);
                    // Restart the limit to start searching for low-hanging fruit after we succeeded
                    // in simplifying it.
                    continue 'outer;
                }
            }
            return result;
        }
    }
}

macro_rules! truth_table {
    ($env:expr, $operator:ident $tt:ident $tf:ident $ft:ident $ff:ident) => {
        $env.add_rewrite_rule(expression!((eq($operator(true)(true))($tt))));
        $env.add_rewrite_rule(expression!((eq($operator(true)(false))($tf))));
        $env.add_rewrite_rule(expression!((eq($operator(false)(true))($ft))));
        $env.add_rewrite_rule(expression!((eq($operator(false)(false))($ff))));
    };
}

fn main() {
    let mut env = Environment::new();
    truth_table!(env, and true false false false);
    truth_table!(env, or true true true false);
    truth_table!(env, imp true false true true);
    truth_table!(env, bicond true false false true);
    env.add_rewrite_rule(expression!((eq(not(true))(false))));
    env.add_rewrite_rule(expression!((eq(not(false))(true))));
    env.add_rewrite_rule(expression!((eq (add a b) (add b a))));
    env.add_rewrite_rule(expression!((eq (mul a b) (mul b a))));
    env.add_rewrite_rule(expression!((eq (sub 0 a) (neg a))));
    env.add_rewrite_rule(expression!((eq (add (add a b) c) (add a (add b c)))));
    env.add_rewrite_rule(expression!((eq (mul (mul a b) c) (mul a (mul b c)))));
    env.add_rewrite_rule(expression!((eq (add (mul x a) (mul y a)) (mul (add x y) a))));
    env.add_rewrite_rule(expression!((eq x (mul 1 x))));
    env.add_rewrite_rule(expression!((eq x (add 0 x))));

    println!("Algebra engine initialized!");

    // let to_rewrite = expression!((and(and(true)(false))(true)));
    let to_rewrite = expression!((add (add a a) (add a (mul a 2))));
    println!("{}", to_rewrite.flatten(&mut env));
    // let to_rewrite = expression!((add (add a a) (add a (mul a 2))));
    // env.simplify(to_rewrite, 5);
    // println!("{:?}", rewritten);
}
