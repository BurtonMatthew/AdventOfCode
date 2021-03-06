use std::collections::VecDeque;
use itertools::Itertools;
use itertools::MinMaxResult::MinMax;

// Simple lines
type InputType = Vec<u64>;
#[aoc_generator(day9)]
pub fn parse_input(buf :&str) -> InputType
{
    buf.lines().map(|l| l.parse().unwrap()).collect()
}

#[aoc(day9, part1)]
pub fn part1(input : &InputType) -> u64
{
    p1(input, 25)

}

pub fn p1(input : &InputType, w:usize) -> u64
{
    let mut sums: VecDeque<u64> =  VecDeque::with_capacity(w*w);

    for i in 0..w
    {
        for j in 0..w
        {
            if i != j
            {
                sums.push_back(input[i] + input[j]);
            }
        }
    }

    for i in w..input.len()
    {
        let v = input[i];
        if sums.contains(&v)
        {
            drop(sums.drain(0..w-1));
            sums.extend(input[i-w+1..i].iter().map(|x| x+v));
        }
        else
        {
            return v;
        }
    }
    unreachable!()
}

#[aoc(day9, part2)]
pub fn part2(input : &InputType) -> u64
{
    p2(input, 25)
}

pub fn p2(input : &InputType, w:usize) -> u64
{
    let p1 = p1(&input,w);

    for run_length in 2..input.len()
    {
        let mut running_total: u64 = input[0..run_length].iter().sum();
        for i in run_length..input.len()
        {
            if running_total != p1
            {
                running_total = running_total + input[i] - input[i-run_length];
            }
            else
            {
                if let MinMax(&min,&max) = &input[i-run_length..i].iter().minmax()
                {
                    return min + max;
                }
            }
        }
    }
    unreachable!()
}

// Credit to Marcela
#[aoc(day9, part2, part2_sliding_window)]
pub fn part2_sliding_window(input : &InputType) -> u64
{
    p2_sliding(input, 25)
}

pub fn p2_sliding(input : &InputType, w:usize) -> u64
{
    let p1 = p1(&input,w);

    let mut start_i = 0;
    let mut running_total = 0;
    for i in 0..input.len()
    {
        let num = input[i];
        while running_total > p1
        {
            running_total -= input[start_i];
            start_i += 1;
        }

        if running_total != p1
        {
            running_total += num
        }
        else
        {
            if let MinMax(&min,&max) = &input[start_i..i].iter().minmax()
            {
                return min + max;
            }
        }
    }
    unreachable!()
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(p1(&parse_input(TEST_DATA), 5), 127)
    }

    #[test]
    pub fn part2_test() 
    {
        assert_eq!(p2(&parse_input(TEST_DATA), 5), 62)
    }
}