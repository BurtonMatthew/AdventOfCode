use text_io::scan;

pub struct Password
{
    min : usize,
    max : usize,
    character : char,
    password : String
}

#[aoc_generator(day2)]
pub fn parse_input(buf : &str) -> Vec<Password>
{
    let mut result = Vec::new();
    for line in buf.lines()
    {
        let min : usize;
        let max : usize;
        let character: char;
        let password: String;
        scan!(line.bytes() => "{}-{} {}: {}", min, max, character, password);
        result.push(Password{min, max, character, password});
    }

    result
}

#[aoc(day2, part1)]
pub fn part1(input : &[Password]) -> usize
{
    input.iter().filter(|pass|
        { 
            let instances = pass.password.chars().filter(|c| *c == pass.character).count(); 
            instances >= pass.min && instances <= pass.max
        }).count()
}

#[aoc(day2, part2)]
pub fn part2(input : &[Password]) -> usize
{
    input.iter().filter(|pass|
        {
            let match_min = pass.password.as_bytes()[pass.min-1] as char == pass.character;
            let match_max = pass.password.as_bytes()[pass.max-1] as char == pass.character;
            match_min ^ match_max
        }).count()
}


#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(&parse_input(TEST_DATA)), 2)
    }

    #[test]
    pub fn part2_test() 
    {
        assert_eq!(part2(&parse_input(TEST_DATA)), 1)
    }
}