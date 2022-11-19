
type InputType = Vec<usize>;
#[aoc_generator(day25)]
pub fn parse_input(buf :&str) -> InputType
{
    buf.lines().map(|s| s.parse().unwrap()).collect()
}

#[aoc(day25, part1)]
pub fn part1(input : &InputType) -> usize
{
    let card_key = input[0];
    let door_key = input[1];

    let subject_number = 7;
    let mut value = 1;
    let mut i = 0;
    while value != card_key
    {
        value *= subject_number;
        value %= 20201227;
        i += 1;
    }

    let subject_number = door_key;
    let mut value = 1;
    let loop_size = i;

    for _ in 0..loop_size
    {
        value *= subject_number;
        value %= 20201227;
    }

    value
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"5764801
17807724";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(&parse_input(TEST_DATA)), 14897079)
    }
}