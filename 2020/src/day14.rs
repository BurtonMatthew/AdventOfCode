use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op
{
    Mask([Option<bool>; 36]),
    Write(usize, usize)
}

// Simple lines
type InputType = Vec<Op>;
#[aoc_generator(day14)]
pub fn parse_input(buf :&str) -> InputType
{
    buf.lines().map(|line|
    {
        if line.starts_with("mask")
        {
            let mut mask = [None; 36];
            for (i,b) in line.chars().skip(7).enumerate()
            {
                match b
                {
                    '0' => { mask[i] = Some(false); }
                    '1' => { mask[i] = Some(true);}
                    _ => {}
                }
            }
            Op::Mask(mask)
        }
        else
        {
            let mut toks = line.split(']');
            let addr = toks.next().unwrap().chars().skip(4).collect::<String>().parse().unwrap();
            let val = toks.next().unwrap().chars().skip(3).collect::<String>().parse().unwrap();
            Op::Write(addr,val)
        }
    }
    ).collect()
}

#[aoc(day14, part1)]
pub fn part1(input : &InputType) -> usize
{
    let mut mem: HashMap<usize, usize> = HashMap::new();
    let mut mask: [Option<bool>; 36] = [None; 36];

    for op in input
    {
        match op
        {
            Op::Mask(m) => { mask = m.clone(); }
            Op::Write(addr, val) =>
            {
                let mut v = *val;
                for(i, b) in mask.iter().rev().enumerate()
                {
                    if let Some(x) = b
                    {
                        match x
                        {
                            false => { v &= !(1 << i ); },
                            true => { v |= 1 << i },
                        }
                    }

                    mem.insert(*addr, v);
                }
            }
        }
    }
    
    mem.values().sum()
}

#[aoc(day14, part2)]
pub fn part2(input : &InputType) -> usize
{
    let mut mem: HashMap<usize, usize> = HashMap::new();
    let mut mask: [Option<bool>; 36] = [None; 36];

    for op in input
    {
        match op
        {
            Op::Mask(m) => { mask = m.clone(); }
            Op::Write(addr, val) =>
            {
                let iterations = 2usize.pow(mask.iter().filter(|b| b.is_none()).count() as u32);
                for ia in 0..iterations
                {
                    let mut floating_addr = *addr;
                    let mut num_xs = 0;
                    for(i, b) in mask.iter().rev().enumerate()
                    {
                        if let Some(x) = b
                        {
                            match x
                            {
                                false => {  },
                                true => { floating_addr |= 1 << i },
                            }
                        }
                        else
                        {
                            match ia & (1 << num_xs) != 0
                            {
                                false => { floating_addr &= !(1 << i ); },
                                true => { floating_addr |= 1 << i },
                            }
                            num_xs += 1;
                        }
                    }

                    mem.insert(floating_addr, *val);
                }
            }
        }
    }
    
    mem.values().sum()
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0";


const TEST_DATA_2: &str = 
"mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(&parse_input(TEST_DATA)), 165)
    }

    #[test]
    pub fn part2_test() 
    {
        assert_eq!(part2(&parse_input(TEST_DATA_2)), 208)
    }
}