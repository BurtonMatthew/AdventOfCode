type InputType = Vec<usize>;
#[aoc_generator(day15)]
pub fn parse_input(buf :&str) -> InputType
{
    buf.split(',').map(|i| i.parse().unwrap()).collect()
}

#[aoc(day15, part1)]
pub fn part1(input : &InputType) -> usize
{
    let mut t = 0;
    let mut spoken = *input.last().unwrap();
    let mut prev_spoken;

    let mut map: Vec<i32> = vec![-1; 2020];
    for i in 0..input.len()
    {
        if i != input.len()-1
        {
            map[input[i]] = t;
        }
        t += 1;
    }

    for t in t..2020
    {
        prev_spoken = spoken;
        if map[spoken as usize] != -1
        {
            let speak = t - map[spoken as usize] -1;
            spoken = speak as usize;
        }
        else
        {
            spoken = 0;
        }

        map[prev_spoken as usize] = t-1;
    }

    spoken
}

#[aoc(day15, part2)]
pub fn part2(input : &InputType) -> usize
{
    let mut t = 0;
    let mut spoken = *input.last().unwrap();
    let mut prev_spoken;

    let mut map: Vec<i32> = vec![-1; 30000000];
    for i in 0..input.len()
    {
        if i != input.len()-1
        {
            map[input[i]] = t;
        }
        t += 1;
    }

    for t in t..30000000
    {
        prev_spoken = spoken;
        if map[spoken as usize] != -1
        {
            let speak = t - map[spoken as usize] -1;
            spoken = speak as usize;
        }
        else
        {
            spoken = 0;
        }

        map[prev_spoken as usize] = t-1;
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