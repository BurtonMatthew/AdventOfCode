use std::mem::swap;

#[derive(Debug, Clone, Copy)]
pub enum Op
{
    N(i32),
    S(i32),
    E(i32),
    W(i32),
    F(i32),
    L(i32),
    R(i32)
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Dir
{
    N,
    S,
    E,
    W
}

type InputType = Vec<Op>;
#[aoc_generator(day12)]
pub fn parse_input(buf :&str) -> InputType
{
    buf.lines().map(|s|
    {
        match s.chars().nth(0).unwrap()
        {
            'N' => Op::N(s[1..].parse().unwrap()),
            'S' => Op::S(s[1..].parse().unwrap()),
            'E' => Op::E(s[1..].parse().unwrap()),
            'W' => Op::W(s[1..].parse().unwrap()),
            'F' => Op::F(s[1..].parse().unwrap()),
            'L' => Op::L(s[1..].parse().unwrap()),
            'R' => Op::R(s[1..].parse().unwrap()),
            _ => unreachable!()
        }
    }).collect()
}

#[aoc(day12, part1)]
pub fn part1(input : &InputType) -> i32
{
    let mut dir = Dir::E;
    let mut x = 0;
    let mut y = 0;

    for op in input
    {
        match op
        {
            Op::N(n) => { y -= n; },
            Op::S(n) => { y += n; },
            Op::W(n) => { x -= n; },
            Op::E(n) => { x += n; },
            Op::F(n) => {
                match dir
                {
                    Dir::N => { y -= n; }
                    Dir::S => { y += n; }
                    Dir::W => { x -= n; }
                    Dir::E => { x += n; }
                }
            },
            Op::R(n) => { dir = rotate(dir, (n/90) % 4) },
            Op::L(n) => { dir = rotate(dir, 4-(n/90) % 4) },
        }
    }

    x.abs() + y.abs()
}

pub fn rotate(dir: Dir, n:i32) -> Dir
{
    let dirs = [Dir::E, Dir::S, Dir::W, Dir::N, Dir::E, Dir::S, Dir::W, Dir::N];
    dirs.iter().cloned().skip_while(|&d| d != dir).skip(n as usize).next().unwrap()
}

#[aoc(day12, part2)]
pub fn part2(input : &InputType) -> i32
{
    let mut x = 0;
    let mut y = 0;
    let mut w_x = 10;
    let mut w_y = -1;

    for op in input
    {
        match op
        {
            Op::N(n) => { w_y -= n; },
            Op::S(n) => { w_y += n; },
            Op::W(n) => { w_x -= n; },
            Op::E(n) => { w_x += n; },
            Op::F(n) => { x += w_x * n; y += w_y * n }
            Op::R(n) => { for _ in 0..n/90 { swap(&mut w_x, &mut w_y); w_x *= -1; } },
            Op::L(n) => { for _ in 0..n/90 { swap(&mut w_x, &mut w_y); w_y *= -1; } },
        }
    }

    x.abs() + y.abs()
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"F10
N3
F7
R90
F11";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(&parse_input(TEST_DATA)), 25)
    }

    #[test]
    pub fn part2_test() 
    {
        assert_eq!(part2(&parse_input(TEST_DATA)), 286)
    }
}