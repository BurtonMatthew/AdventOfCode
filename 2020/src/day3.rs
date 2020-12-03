use text_io::scan;

/*
#[aoc_generator(day3)]
pub fn parse_input(buf : &str) -> &str
{
    buf
}*/

#[aoc(day3, part1)]
pub fn part1(input : &str) -> usize
{
    let i : Vec<&str> = input.lines().collect();
    slopes(&i, 3, 1)
}

fn slopes(i: &Vec<&str>, r: usize, d: usize) -> usize
{
    let mut x = 0;
    let mut y = 0;
    let mut c = 0;
    let w = i[0].len();
    let h = i.len();

    while(y < h)
    {
        if i[y].chars().nth(x).unwrap() == '#' { c += 1; }
        x = (x+r) % w;
        y += d;
    }
    c
}

#[aoc(day3, part2)]
pub fn part2(input : &str) -> usize
{
    let i : Vec<&str> = input.lines().collect();
    slopes(&i, 1, 1) * slopes(&i, 3, 1) * slopes(&i, 5, 1) * slopes(&i, 7, 1) * slopes(&i, 1, 2)
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
        assert_eq!(part1(&parse_input(TEST_DATA)), 2)
    }

    #[test]
    pub fn part2_test() 
    {
        assert_eq!(part2(&parse_input(TEST_DATA)), 1)
    }
}