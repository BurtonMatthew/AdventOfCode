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
    let mut new_prog = input.clone();
    for i in 0..input.len()
    {
        if let Ops::Nop(x) = input[i]
        {
            new_prog[i] = Ops::Jmp(x);
            if let Some(r) = part2_prog(&new_prog)
            {
                return r;
            }
            new_prog[i] = Ops::Nop(x);
        }
        else if let Ops::Jmp(x) = input[i]
        {
            new_prog[i] = Ops::Nop(x);
            if let Some(r) = part2_prog(&new_prog)
            {
                return r;
            }
            new_prog[i] = Ops::Jmp(x);
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


#[aoc(day8, part2, part2_rollback)]
pub fn part2_rollback(input : &InputType) -> i32
{
    struct State
    {
        i: usize,
        acc: i32,
        visited: HashSet<usize>
    }
    let mut visited: HashSet<usize> = HashSet::new();
    let mut states: Vec<State> = Vec::new();
    let mut i = 0;
    let mut acc = 0;
    
    // Run unmodified program until we hit loop, save full machine state at every nop/jmp
    while !visited.contains(&i)
    {
        visited.insert(i);
        match &input[i]
        {
            Ops::Nop(_) => { states.push(State{i,acc,visited: visited.clone()}); i += 1;},
            Ops::Acc(x) => { acc += x; i +=1; },
            Ops::Jmp(x) => { states.push(State{i,acc,visited: visited.clone()}); i = (i as i32 + x) as usize; }
        }
    }

    // All nops/jumps prior to the loops are candidates to be swapped. Work backwards from the ones we encountered
    // since they're the quickest to terminate
    while let Some(state) = states.pop()
    {
        i = state.i;
        acc = state.acc;
        // Swap jmp and nop for first instruction
        match &input[i]
        {
            Ops::Jmp(_) => { i += 1;},
            Ops::Acc(x) => { acc += x; i +=1; },
            Ops::Nop(x) => { i = (i as i32 + x) as usize; }
        }
        visited = state.visited;

        // Run the machine until we hit the loop or an exit successfully
        while !visited.contains(&i) && i < input.len()
        {
            visited.insert(i);
            match &input[i]
            {
                Ops::Nop(_) => { i += 1;},
                Ops::Acc(x) => { acc += x; i +=1; },
                Ops::Jmp(x) => { i = (i as i32 + x) as usize; }
            }
        }

        // If successful, return the result
        if i >= input.len() { return acc; }
    }

    unreachable!()
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