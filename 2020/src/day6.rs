use itertools::Itertools;
use std::collections::HashSet;

#[aoc_generator(day6)]
pub fn parse_input(buf :&str) -> Vec<String>
{
    buf.split("\n\n").map(|s| s.to_string()).collect()
}

#[aoc(day6, part1)]
pub fn part1(input : &Vec<String>) -> usize
{
    input.iter().map(|s| 
        {
            s.chars().filter(|c| !c.is_whitespace()).unique().count()
        }).sum()
}

#[aoc(day6, part2)]
pub fn part2(input : &Vec<String>) -> usize
{
    input.iter().map(|s| 
    {
        let sets = s.lines().map(|l| l.to_string()).collect::<Vec<String>>();

        let mut inter = sets[0].clone();
        for i in 1..sets.len()
        {
            inter = intersect(&inter, &sets[i]);
        }

        inter.len()
    }).sum()
}

pub fn intersect(a:&String, b:&String) -> String
{
    a.chars().filter(|c| b.contains(*c)).collect()
}

#[aoc(day6, part2, part2_hashset)]
pub fn part2_hashset(input : &Vec<String>) -> usize
{
    input.iter().map(|s| 
    {
        let mut set_iter = s.lines().map(|l| l.chars().collect::<HashSet<char>>());

        set_iter.next()
            .map(|set| set_iter.fold(set, |set1, set2| set1.intersection(&set2).cloned().collect()))
            .unwrap()
            .len()
    }).sum()
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"abc

a
b
c

ab
ac

a
a
a
a

b";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(&parse_input(TEST_DATA)), 11)
    }

    #[test]
    pub fn part2_test() 
    {
        assert_eq!(part2(&parse_input(TEST_DATA)), 6)
    }
}