use itertools::Itertools;

#[aoc_generator(day5)]
pub fn parse_input(buf :&str) -> Vec<usize>
{
    buf.lines().map(|line|
        {
            let bin_str : String = line.chars().map(|c| 
                match c
                {
                    'F' => '0',
                    'B' => '1',
                    'L' => '0',
                    'R' => '1',
                    _ => unreachable!()
                }
                ).collect();
    
            usize::from_str_radix(&bin_str[0..7], 2).unwrap() * 8 + usize::from_str_radix(&bin_str[7..10], 2).unwrap()
        }).sorted().collect()
}

#[aoc(day5, part1)]
pub fn part1(input : &[usize]) -> usize
{
    *input.iter().max().unwrap()
}

#[aoc(day5, part2)]
pub fn part2(input : &[usize]) -> usize
{
    (1..(128*8)).filter(|id| !input.contains(&id) && input.contains(&(id-1)) && input.contains(&(id+1))).next().unwrap()
}

#[aoc(day5, part2, part2_single_pass)]
pub fn part2_single_pass(input : &[usize]) -> usize
{
    for i in 0..input.len()
    {
        if input[i]+1 != input[i+1] { return input[i]+1; }
    }
    unreachable!()
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"FBFBBFFRLR
BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL";

    #[test]
    pub fn parse_test() 
    {
        assert_eq!(parse_input(TEST_DATA), vec!(119, 357, 567, 820));
    }

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(&parse_input(TEST_DATA)), 820);
    }
}