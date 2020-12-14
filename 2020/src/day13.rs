pub struct Input
{
    depart: usize,
    busses: Vec<Option<usize>>
}

type InputType = Input;
#[aoc_generator(day13)]
pub fn parse_input(buf :&str) -> InputType
{
    let mut iter = buf.lines();
    let depart = iter.next().unwrap().parse().unwrap();
    let busses = iter.next().unwrap().split(|c| c == ',').map(|c| c.parse().ok()).collect();

    Input { depart, busses }
}

#[aoc(day13, part1)]
pub fn part1(input : &InputType) -> usize
{
    let bus = input.busses.iter()
        .filter(|b| !b.is_none())
        .map(|b| b.unwrap())
        .map(|b| { let mut c = b; while c < input.depart { c+=b;} (c,b) })
        .min_by(|(c,_), (c1,_)| c.cmp(c1))
        .unwrap();
    bus.1 * (bus.0 - input.depart)
}

#[aoc(day13, part2)]
pub fn part2(input : &InputType) -> i64
{
    let mut residues: Vec<i64> = Vec::new();
    let mut modulii: Vec<i64> = Vec::new();

    for (i, bus) in input.busses.iter().enumerate()
    {
        if let Some(x) = bus
        {
            modulii.push(*x as i64);
            residues.push(*x as i64 - i as i64);
        }
    }

    chinese_remainder(&residues, &modulii).unwrap()
}

#[aoc(day13, part2, part2_lcm)]
pub fn part2_lcm(input : &InputType) -> usize
{
    let mut t = 0;
    let mut period = input.busses[0].unwrap(); // todo: handle first bus being bad?
    let mut i = 1;
    while i < input.busses.len()
    {
        // Skip past non-busses
        if input.busses[i].is_none() 
        { 
            i += 1; 
            continue;
        }
        let bus = input.busses[i].unwrap();
        if (t+i) % bus == 0
        {
            period = lcm(period,bus);
            i += 1;
        }
        else
        {
            t = t + period;
        }
    }

    t
}

// Rosetta code
use std::cmp::{max, min};
 
fn gcd(a: usize, b: usize) -> usize {
    match ((a, b), (a & 1, b & 1)) {
        ((x, y), _) if x == y => y,
        ((0, x), _) | ((x, 0), _) => x,
        ((x, y), (0, 1)) | ((y, x), (1, 0)) => gcd(x >> 1, y),
        ((x, y), (0, 0)) => gcd(x >> 1, y >> 1) << 1,
        ((x, y), (1, 1)) => {
            let (x, y) = (min(x, y), max(x, y));
            gcd((y - x) >> 1, x)
        }
        _ => unreachable!(),
    }
}
 
fn lcm(a: usize, b: usize) -> usize {
    a * b / gcd(a, b)
}

fn egcd(a: i64, b: i64) -> (i64, i64, i64) {
    if a == 0 {
        (b, 0, 1)
    } else {
        let (g, x, y) = egcd(b % a, a);
        (g, y - (b / a) * x, x)
    }
}
 
fn mod_inv(x: i64, n: i64) -> Option<i64> {
    let (g, x, _) = egcd(x, n);
    if g == 1 {
        Some((x % n + n) % n)
    } else {
        None
    }
}
 
fn chinese_remainder(residues: &[i64], modulii: &[i64]) -> Option<i64> {
    let prod = modulii.iter().product::<i64>();
 
    let mut sum = 0;
 
    for (&residue, &modulus) in residues.iter().zip(modulii) {
        let p = prod / modulus;
        sum += residue * mod_inv(p, modulus)? * p
    }
 
    Some(sum % prod)
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"939
7,13,x,x,59,x,31,19";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(&parse_input(TEST_DATA)), 295)
    }

    #[test]
    pub fn part2_test() 
    {
        assert_eq!(part2(&parse_input(TEST_DATA)), 1068781)
    }
}