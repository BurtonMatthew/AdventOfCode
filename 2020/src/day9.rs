use std::collections::VecDeque;

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
    let mut vector: VecDeque<u64> = VecDeque::with_capacity(w);
    for i in 0..w
    {
        vector.push_back(input[i]);
    }

    for i in w..input.len()
    {
        let v = input[i];
        let mut found = false;
        for j in 0..w
        {
            for k in 0..w
            {
                if !found && j != k && v == (vector[j] + vector[k])
                {
                    vector.pop_front();
                    vector.push_back(v);
                    found = true;
                }
            }
        }

        if !found
        {
            return v;
        }
    }
    unreachable!()
}

#[aoc(day9, part2)]
pub fn part2(input : &InputType) -> u64
{
    let p1 = part1(&input);
    for i in 0..input.len()
    {
        let mut sum = 0;
        let mut min = u64::MAX;
        let mut max = u64::MIN;

        for j in i..input.len()
        {
            if input[j] < min { min = input[j];}
            if input[j] > max { max = input[j];}

            sum += input[j];
            if sum == p1 && j != i
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
        assert_eq!(part2(&parse_input(TEST_DATA)), 62)
    }
}