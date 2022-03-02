mod fold_addition;
mod fold_multiplication;

use super::Simplifiers;
use crate::simplifiers;

pub fn simplifiers() -> Simplifiers {
    simplifiers![
        fold_multiplication::SFoldMultiplication,
        fold_addition::SFoldAddition,
    ]
}
