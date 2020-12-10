use itertools::Itertools;

type InputType = Vec<usize>;
#[aoc_generator(day10)]
pub fn parse_input(buf :&str) -> InputType
{
    let mut v = vec![0;1];
    v.extend(buf.lines().map(|l| l.parse().unwrap()).sorted().collect::<InputType>());
    v
}


#[aoc(day10, part1)]
pub fn part1(input : &InputType) -> usize
{
    let mut gap1 = 0;
    let mut gap3 = 0;
    for i in 0..input.len()-1
    {
        if input[i+1] - input[i] == 1 { gap1 += 1; }
        if input[i+1] - input[i] == 3 { gap3 += 1; }
    }

    gap1 * (gap3+1)
}

#[aoc(day10, part2)]
pub fn part2(input : &InputType) -> usize
{
    let mut combinations = vec![0; input.last().unwrap()+4];
    combinations[input.last().unwrap()+3] = 1;
    for &x in input.iter().rev()
    {
        combinations[x] = combinations[x+1] + combinations[x+2] + combinations[x+3];
    }

    combinations[0]
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"16
10
15
5
1
11
7
19
6
12
4";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(&parse_input(TEST_DATA)), 35)
    }

    #[test]
    pub fn part2_test() 
    {
        assert_eq!(part2(&parse_input(TEST_DATA)), 8)
    }
}