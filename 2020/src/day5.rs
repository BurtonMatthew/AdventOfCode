#[aoc(day5, part1)]
pub fn part1(input : &str) -> usize
{
    input.lines().map(|line|
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
    }).max().unwrap()
}

#[aoc(day5, part2)]
pub fn part2(input :  &str) -> usize
{
    let seats: Vec<usize> = input.lines().map(|line|
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
    }).collect();

    for id in 1..(128*8)
    {
        if !seats.contains(&id) && seats.contains(&(id-1)) && seats.contains(&(id+1)) { return id }
    }
    unreachable!()
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"FBFBBFFRLR";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(TEST_DATA), 357);
    }
}