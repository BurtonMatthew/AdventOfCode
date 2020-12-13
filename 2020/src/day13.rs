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
            residues.push((x - i) as i64);
        }
    }

    chinese_remainder(&residues, &modulii).unwrap()
}

// Rosetta code
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
7,13,0,0,59,0,31,19";

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