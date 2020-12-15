use std::collections::HashMap;

type InputType = Vec<usize>;
#[aoc_generator(day15)]
pub fn parse_input(buf :&str) -> InputType
{
    buf.split(',').map(|i| i.parse().unwrap()).collect()
}

#[aoc(day15, part1)]
pub fn part1(input : &InputType) -> usize
{
    let mut map = HashMap::new();

    let mut t = 0;
    let mut spoken = *input.last().unwrap();
    let mut prev_spoken = spoken;

    for i in 0..input.len()
    {
        if i != input.len()-1
        {
            map.insert(input[i], t);
        }
        t += 1;
    }

    for t in t..2020
    {
        prev_spoken = spoken;
        if map.contains_key(&spoken)
        {
            let speak = t - map.get(&spoken).unwrap() -1;
            spoken = speak;
        }
        else
        {
            spoken = 0;
        }

        map.insert(prev_spoken, t-1);
    }

    spoken
}

#[aoc(day15, part2)]
pub fn part2(input : &InputType) -> usize
{
    let mut map = HashMap::new();

    let mut t = 0;
    let mut spoken = *input.last().unwrap();
    let mut prev_spoken = spoken;

    for i in 0..input.len()
    {
        if i != input.len()-1
        {
            map.insert(input[i], t);
        }
        t += 1;
    }

    for t in t..30000001
    {
        prev_spoken = spoken;
        if map.contains_key(&spoken)
        {
            let speak = t - map.get(&spoken).unwrap() -1;
            spoken = speak;
        }
        else
        {
            spoken = 0;
        }

        map.insert(prev_spoken, t-1);
    }

    spoken
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"0,3,6";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(&parse_input(TEST_DATA)), 436)
    }

    #[test]
    pub fn part2_test() 
    {
        assert_eq!(part2(&parse_input(TEST_DATA)), 175594)
    }
}