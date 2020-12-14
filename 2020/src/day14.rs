use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op
{
    Mask(usize, usize, usize),
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
            let mut clear_mask = 0;
            let mut set_mask = 0;
            let mut x_mask = 0;

            for (i,b) in line.chars().skip(7).enumerate()
            {
                match b
                {
                    '0' => { clear_mask |= 1 << (35-i); }
                    '1' => { set_mask |= 1 << (35-i); }
                    'X' => { x_mask |= 1 << (35-i); }
                    _ => unreachable!()
                }
            }
            Op::Mask(clear_mask, set_mask, x_mask)
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
    let mut clear_mask = 0;
    let mut set_mask = 0;

    for op in input
    {
        match op
        {
            Op::Mask(clr, set, _) => { clear_mask = *clr; set_mask = *set; }
            Op::Write(addr, val) => { mem.insert(*addr, val & !clear_mask | set_mask); }
        }
    }
    
    mem.values().sum()
}

#[aoc(day14, part2, part2_addr_list)]
pub fn part2_addr_list(input : &InputType) -> usize
{
    let mut mem: HashMap<usize, usize> = HashMap::new();
    let mut set_mask = 0;
    let mut x_mask = 0;

    for op in input
    {
        match op
        {
            Op::Mask(_, set, x) => { set_mask = *set; x_mask = *x; }
            Op::Write(addr, val) =>
            {
                let bits = 2usize.pow(x_mask.count_ones());
                let mut addresses = Vec::with_capacity(bits);
                addresses.push(addr | set_mask);
                for bit in 0..36
                {
                    if x_mask & (1 << bit) != 0
                    {
                        for i in 0..addresses.len()
                        {
                            let set = 1 << bit;
                            let clr = !set;
                            addresses.push(addresses[i] | set);
                            addresses[i] &= clr;
                        }
                    }
                }
                for address in addresses
                {
                    mem.insert(address, *val);
                }
            }
        }
    }
    
    mem.values().sum()
}

#[aoc(day14, part2, part2_xmask_cache)]
pub fn part2_xmask_cache(input : &InputType) -> usize
{
    let mut mem: HashMap<usize, usize> = HashMap::new();
    let mut set_mask = 0;
    let mut x_masks: Vec<(usize, usize)> = Vec::with_capacity(1024);

    for op in input
    {
        match op
        {
            Op::Mask(_, set, x_mask) => 
            { 
                set_mask = *set; 
                x_masks.clear();

                for bit in 0..36
                {
                    if x_mask & (1 << bit) != 0
                    {
                        let bit_mask = 1 << bit;

                        if x_masks.len() == 0
                        {
                            x_masks.push((0, bit_mask));
                            x_masks.push((bit_mask, 0));
                        }
                        else
                        {

                            for i in 0..x_masks.len()
                            {
                                let old_mask = x_masks[i];
                                x_masks[i] = (old_mask.0 | bit_mask, old_mask.1);
                                x_masks.push((old_mask.0, old_mask.1 | bit_mask));
                            }
                        }
                    }
                }
            }
            Op::Write(addr, val) =>
            {
                let masked_addr = addr | set_mask;
                for (clr, set) in &x_masks
                {
                    mem.insert(masked_addr & !clr | set, *val);
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