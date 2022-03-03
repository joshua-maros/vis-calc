use std::{
    fmt::{self, Display, Formatter},
    hash::{Hash, Hasher},
    ops::{Div, DivAssign, Mul, MulAssign, Rem, RemAssign},
};

use derive_more::*;

#[derive(Clone, Copy, PartialOrd, Neg, Add, AddAssign, Sub, SubAssign, Sum)]
pub struct Number(f64);

impl Number {
    pub fn powf(self, other: Self) -> Self {
        Self(self.0.powf(other.0))
    }

    pub fn as_int(&self) -> Option<i32> {
        let as_int = self.0 as i32;
        if as_int as f64 == self.0 {
            Some(as_int)
        } else {
            None
        }
    }
}

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
