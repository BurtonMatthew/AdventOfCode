use std::str::FromStr;
use std::collections::HashSet;

#[derive(Debug, Clone, Copy)]
pub enum Ops
{
    Nop(i32),
    Acc(i32),
    Jmp(i32)
}

type InputType = Vec<Ops>;
#[aoc_generator(day8)]
pub fn parse_input(buf :&str) -> InputType
{
    buf.lines().map(|l|
    {
        let mut toks = l.split(" ");
        match toks.next().unwrap()
        {
            "nop" => Ops::Nop(i32::from_str(toks.next().unwrap()).unwrap()),
            "acc" => Ops::Acc(i32::from_str(toks.next().unwrap()).unwrap()),
            "jmp" => Ops::Jmp(i32::from_str(toks.next().unwrap()).unwrap()),
            _ => unreachable!()
        }
    }).collect()
}

#[aoc(day8, part1)]
pub fn part1(input : &InputType) -> i32
{
    let mut visited :HashSet<usize> = HashSet::new();
    let mut i = 0;
    let mut acc = 0;
    
    while !visited.contains(&i)
    {
        visited.insert(i);
        match &input[i]
        {
            Ops::Nop(_) => { i += 1;},
            Ops::Acc(x) => { acc += x; i +=1; },
            Ops::Jmp(x) => { i = (i as i32 + x) as usize; }

        }
    }

    acc
}

#[aoc(day8, part2)]
pub fn part2(input : &InputType) -> i32
{
    for i in 0..input.len()
    {
        let mut new_prog = input.clone();
        if let Ops::Nop(x) = input[i]
        {
            new_prog[i] = Ops::Jmp(x);
            if let Some(r) = part2_prog(&new_prog)
            {
                return r;
            }
        }
        else if let Ops::Jmp(x) = input[i]
        {
            new_prog[i] = Ops::Nop(x);
            if let Some(r) = part2_prog(&new_prog)
            {
                return r;
            }
        }
    }
    unreachable!()
}

pub fn part2_prog(input : &InputType) -> Option<i32>
{
    let mut visited :HashSet<usize> = HashSet::new();
    let mut i = 0;
    let mut acc = 0;
    
    while !visited.contains(&i)
    {
        if i == input.len() { return Some(acc); }
        visited.insert(i);
        match &input[i]
        {
            Ops::Nop(_) => { i += 1;},
            Ops::Acc(x) => { acc += x; i +=1; },
            Ops::Jmp(x) => { i = (i as i32 + x) as usize; }
        }
    }

    None
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(&parse_input(TEST_DATA)), 5)
    }

    #[test]
    pub fn part2_test() 
    {
        assert_eq!(part2(&parse_input(TEST_DATA)), 8)
    }
}