mod normalize;

use super::Simplifiers;
use crate::simplifiers;

pub fn simplifiers() -> Simplifiers {
    simplifiers![normalize::SNormalize,]
}
