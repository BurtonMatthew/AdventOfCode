use std::str::FromStr;
use std::collections::HashSet;
use std::collections::VecDeque;
use itertools::Itertools;
use text_io::scan;
use vec2::Vec2;
use pathfinding::prelude::{astar};
use std::collections::HashMap;

#[aoc(dayX, part1)]
pub fn part1(input : &InputType) -> usize
{
    0
}

#[aoc(dayX, part2)]
pub fn part2(input : &InputType) -> usize
{
    0
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(&parse_input(TEST_DATA)), 0)
    }

    #[test]
    pub fn part2_test() 
    {
        assert_eq!(part2(&parse_input(TEST_DATA)), 0)
    }
}