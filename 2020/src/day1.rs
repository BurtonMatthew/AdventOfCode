use itertools::Itertools;
use std::cmp::Ordering;

#[aoc_generator(day1)]
pub fn parse_input(buf : &str) -> Vec<i32>
{
    buf.lines()
        .map(|line| line.parse().unwrap())
        .sorted()
        .collect()
}

#[aoc(day1, part1)]
pub fn part1(input : &[i32]) -> i32
{
    find_product_of_summands_matching_target_sorted(&input, 0, input.len()-1, 2020).unwrap()
}

#[aoc(day1, part2)]
pub fn part2(input : &[i32]) -> i32
{
        (0..input.len())
            .filter_map(|i| find_product_of_summands_matching_target_sorted(&input, i+1, input.len()-1, 2020-input[i]).map(|x| x * input[i]))
            .next()
            .unwrap()
}

pub fn find_product_of_summands_matching_target_sorted(arr: &[i32], start: usize, end: usize, target:i32) -> Option<i32>
{
    if start < end
    {
        match (arr[start] + arr[end]).cmp(&target)
        {
            Ordering::Less => find_product_of_summands_matching_target_sorted(arr, start+1, end, target),
            Ordering::Equal => Some(arr[start] * arr[end]),
            Ordering::Greater => find_product_of_summands_matching_target_sorted(arr, start, end-1, target)
        }
    }
    else
    {
        None
    }
}

#[cfg(test)]
mod tests 
{
    use super::*;
    const TEST_DATA: [i32; 6] = [1721, 979, 366, 299, 675, 1456];

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(&TEST_DATA), 1721 * 299)
    }

    #[test]
    pub fn part2_test() 
    {
        assert_eq!(part2(&TEST_DATA), 979 * 366 * 675)
    }
}