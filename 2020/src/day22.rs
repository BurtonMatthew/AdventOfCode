use std::collections::VecDeque;
use std::collections::hash_map::DefaultHasher;
use std::hash::Hasher;
use std::hash::Hash;

type InputType = (VecDeque<u32>, VecDeque<u32>);
#[aoc_generator(day22)]
pub fn parse_input(buf :&str) -> InputType
{
    let sanitized_buf = buf.replace("\r","");
    let mut blocks = sanitized_buf.split("\n\n");
    (blocks.next().unwrap().lines().skip(1).map(|i| i.parse().unwrap()).collect(), blocks.next().unwrap().lines().skip(1).map(|i| i.parse().unwrap()).collect())
}

#[aoc(day22, part1)]
pub fn part1(input : &InputType) -> usize
{
    let (mut p1, mut p2) = input.clone();
    while !p1.is_empty() && !p2.is_empty()
    {
        let p1top = p1.pop_front().unwrap();
        let p2top = p2.pop_front().unwrap();

        if p1top > p2top
        {
            p1.push_back(p1top);
            p1.push_back(p2top);
        }
        else
        {
            p2.push_back(p2top);
            p2.push_back(p1top);
        }
    }
    
    let winning = if !p1.is_empty() {p1} else {p2};
    winning.into_iter().rev().enumerate().map(|(i, card)| (i+1) * card as usize).sum()
}

#[aoc(day22, part2)]
pub fn part2(input : &InputType) -> usize
{
    let (mut p1, mut p2) = input.clone();
    let mut prev = Vec::new();
    recursive_combat(&mut p1, &mut p2, &mut prev, false);
    let winning = if !p1.is_empty() {p1} else {p2};
    winning.into_iter().rev().enumerate().map(|(i, card)| (i+1) * card as usize).sum()
}

pub fn recursive_combat(p1: &mut VecDeque<u32>, p2: &mut VecDeque<u32>, prev: &mut Vec<u64>, is_subgame: bool) -> bool
{
    if is_subgame && p1.iter().max().unwrap() > p2.iter().max().unwrap()
    {
        return true;
    }
    while !p1.is_empty() && !p2.is_empty()
    {
        let mut hasher = DefaultHasher::new();
        p1.hash(&mut hasher);
        p2.hash(&mut hasher);
        let hash = hasher.finish();
        if prev.contains(&hash)
        {
            return true;
        }
        else
        {
            prev.push(hash);
        }

        let p1top = p1.pop_front().unwrap();
        let p2top = p2.pop_front().unwrap();

        if p1top as usize <= p1.len() && p2top as usize <= p2.len()
        {
            if recursive_combat(&mut p1.iter().take(p1top as usize).cloned().collect(), &mut p2.iter().take(p2top as usize).cloned().collect(), &mut Vec::new(), true)
            {
                p1.push_back(p1top);
                p1.push_back(p2top);
            }
            else
            {
                p2.push_back(p2top);
                p2.push_back(p1top);
            }
        }
        else if p1top > p2top
        {
            p1.push_back(p1top);
            p1.push_back(p2top);
        }
        else
        {
            p2.push_back(p2top);
            p2.push_back(p1top);
        }
    }

    !p1.is_empty()
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10";

    const TEST_DATA_INF: &str =
"Player 1:
43
19

Player 2:
2
29
14";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(&parse_input(TEST_DATA)), 306)
    }

    #[test]
    pub fn part2_test() 
    {
        assert_eq!(part2(&parse_input(TEST_DATA)), 291);
        part2(&parse_input(TEST_DATA_INF));
    }
}