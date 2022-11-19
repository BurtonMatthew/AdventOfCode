#![allow(dead_code)]
use std::convert::{From};
use std::ops::{Add, AddAssign, Sub, SubAssign, Mul, MulAssign, Div, DivAssign, Neg};
use std::fmt::{Debug,Display};


#[repr(transparent)]
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct ModInteger<T, const MOD: i128>
    where T : num::Integer + Copy
{
    value: T
}

impl<T, const MOD: i128> ModInteger<T, {MOD}>
    where T: num::Integer + Copy + std::convert::From<i128>
{
    fn modulo(val: T) -> Self
    {
        Self { value: (val % MOD.into() + MOD.into()) % MOD.into() }
    }

    pub fn pow(&self, exp: T) -> Self
    {
        Self::modulo(mod_pow(self.value, exp, MOD))
    }
}

impl<T, const MOD: i128> Add for ModInteger<T, {MOD}>
    where T : num::Integer + Copy + std::convert::From<i128>
{
    type Output = Self;

    fn add(self, other: Self) -> Self::Output 
    {
        ModInteger::modulo(self.value + other.value)
    }
}

impl<T, const MOD: i128> Add<T> for ModInteger<T, {MOD}>
    where T : num::Integer + Copy + std::convert::From<i128>
{
    type Output = Self;

    fn add(self, other: T) -> Self::Output 
    {
        ModInteger::modulo(self.value + other)
    }
}

impl<T, const MOD: i128> AddAssign for ModInteger<T, {MOD}>
    where T : num::Integer + Copy + std::convert::From<i128>
{
    fn add_assign(&mut self, other: Self)
    {
        *self = other + *self;
    }
}

impl<T, const MOD: i128> Sub for ModInteger<T, {MOD}>
    where T : num::Integer + Copy + std::convert::From<i128>
{
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output 
    {
        ModInteger::modulo(self.value - other.value)
    }
}

impl<T, const MOD: i128> Sub<T> for ModInteger<T, {MOD}>
    where T : num::Integer + Copy + std::convert::From<i128>
{
    type Output = Self;

    fn sub(self, other: T) -> Self::Output 
    {
        ModInteger::modulo(self.value - other)
    }
}

impl<T, const MOD: i128> SubAssign for ModInteger<T, {MOD}>
    where T : num::Integer + Copy + std::convert::From<i128>
{
    fn sub_assign(&mut self, other: Self)
    {
        *self = other - *self;
    }
}

impl<T, const MOD: i128> Mul for ModInteger<T, {MOD}>
    where T : num::Integer + Copy + std::convert::From<i128>
{
    type Output = Self;

    fn mul(self, other: Self) -> Self::Output 
    {
        ModInteger::modulo(self.value * other.value)
    }
}

impl<T, const MOD: i128> Mul<T> for ModInteger<T, {MOD}>
    where T : num::Integer + Copy + std::convert::From<i128>
{
    type Output = Self;

    fn mul(self, other: T) -> Self::Output 
    {
        ModInteger::modulo(self.value * other)
    }
}

impl<T, const MOD: i128> MulAssign for ModInteger<T, {MOD}>
    where T : num::Integer + Copy + std::convert::From<i128>
{
    fn mul_assign(&mut self, other: Self)
    {
        *self = other * *self;
    }
}

impl<T, const MOD: i128> Div for ModInteger<T, {MOD}>
    where T : num::Integer + Copy + std::convert::From<i128>
{
    type Output = Self;

    fn div(self, other: Self) -> Self::Output 
    {
        ModInteger::modulo(self.value * mod_inv(other.value, MOD))
    }
}

impl<T, const MOD: i128> Div<T> for ModInteger<T, {MOD}>
    where T : num::Integer + Copy + std::convert::From<i128>
{
    type Output = Self;

    fn div(self, other: T) -> Self::Output 
    {
        ModInteger::modulo(self.value * mod_inv(other, MOD))
    }
}

impl<T, const MOD: i128> DivAssign for ModInteger<T, {MOD}>
    where T : num::Integer + Copy + std::convert::From<i128>
{
    fn div_assign(&mut self, other: Self)
    {
        *self = other / *self;
    }
}

impl<T, const MOD: i128> Neg for ModInteger<T, {MOD}>
    where T: num::Integer + Copy + std::convert::From<i128>
{
    type Output = Self;
    fn neg(self) -> Self::Output 
    {
        Self { value: T::from(MOD) - self.value - T::one() }
    }
}

impl<const MOD: i128> From<i128> for ModInteger<i128, {MOD}>
{
    fn from(item: i128) -> Self
    {
        ModInteger::modulo(item)
    }
}

impl<const MOD: i128> From<ModInteger<i128, {MOD}>> for i128
{
    fn from(item: ModInteger<i128, {MOD}>) -> Self
    {
        item.value
    }
}

impl<T, const MOD: i128> Debug for ModInteger<T, {MOD}>
    where T: Debug + Copy + num::Integer
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result 
    {
        self.value.fmt(f)
    }
}

impl<T, const MOD: i128> Display for ModInteger<T, {MOD}>
    where T: Display + Copy + num::Integer
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result 
    {
        self.value.fmt(f)
    }
}

fn mod_inv<T>(a: T, module: i128) -> T 
    where T: Copy + std::convert::From<i128> + num::Integer + num::Zero + num::One
{
    let mut mn: (T,T) = (module.into(), a);
    let mut xy: (T,T) = (T::zero(), T::one());
   
    while mn.1 != T::zero() {
      xy = (xy.1, xy.0 - (mn.0 / mn.1) * xy.1);
      mn = (mn.1, mn.0 % mn.1);
    }
   
    while xy.0 < T::zero() {
      xy.0 = xy.0 + module.into();
    }
    xy.0
}

fn mod_pow<T>(mut base: T, mut exp: T, modulus: i128) -> T 
    where T: Copy + std::convert::From<i128> + num::Integer + num::Zero + num::One
{
    if modulus == 1 { return T::zero() }
    let mut result = T::one();
    base = base % modulus.into();
    while exp > T::zero() {
        if exp % T::from(2) == T::one() {
            result = result * base % modulus.into();
        }
        exp = exp / T::from(2);
        base = base * base % modulus.into()
    }
    result
}