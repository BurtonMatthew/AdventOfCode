use itertools::Itertools;

type InputType = Vec<usize>;
#[aoc_generator(day10)]
pub fn parse_input(buf :&str) -> InputType
{
    let mut v = vec![0;1];
    v.extend(buf.lines().map(|l| l.parse::<usize>().unwrap()).sorted());
    v
}


#[aoc(day10, part1)]
pub fn part1(input : &InputType) -> usize
{
    let mut gaps = [0;4];
    for i in 0..input.len()-1
    {
        gaps[input[i+1] - input[i]] += 1;
    }
    gaps[1] * (gaps[3]+1)
}

#[aoc(day10, part2)]
pub fn part2(input : &InputType) -> usize
{
    let mut paths_to_output = vec![0; input.last().unwrap()+3];
    paths_to_output[*input.last().unwrap()] = 1;
    for &x in input.iter().rev().skip(1)
    {
        paths_to_output[x] = paths_to_output[x+1] + paths_to_output[x+2] + paths_to_output[x+3];
    }

    paths_to_output[0]
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