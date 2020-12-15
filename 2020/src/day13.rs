use itertools::Itertools;

pub struct Input
{
    depart: usize,
    busses: Vec<Option<usize>>
}

type InputType = Input;
#[aoc_generator(day13)]
pub fn parse_input(buf :&str) -> InputType
{
    let mut iter = buf.lines();
    let depart = iter.next().unwrap().parse().unwrap();
    let busses = iter.next().unwrap().split(|c| c == ',').map(|c| c.parse().ok()).collect();

    Input { depart, busses }
}

#[aoc(day13, part1)]
pub fn part1(input : &InputType) -> usize
{
    let bus = input.busses.iter()
        .filter(|b| !b.is_none())
        .map(|b| b.unwrap())
        .map(|b| { let mut c = b; while c < input.depart { c+=b;} (c,b) })
        .min_by(|(c,_), (c1,_)| c.cmp(c1))
        .unwrap();
    bus.1 * (bus.0 - input.depart)
}

#[aoc(day13, part2)]
pub fn part2(input : &InputType) -> isize
{
    let mut t = 0;
    let mut period = 0;
    let mut indexed_busses_sorted = input.busses.iter().enumerate().filter_map(|(i, &b)| Some((i as isize,b? as isize))).sorted_by(|(_,b1), (_,b2)| b2.cmp(b1));
    if let Some((i,b)) =  indexed_busses_sorted.next()
    {
        t = t-i;
        period = b;
    }

    for (i,b) in indexed_busses_sorted
    {
        while (t+i) % b != 0
        {
            t = t + period;
        }
        period = period *b; // Should LCM but all busses prime
    }

    t
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"939
7,13,x,x,59,x,31,19";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(&parse_input(TEST_DATA)), 295)
    }

    #[test]
    pub fn part2_test() 
    {
        assert_eq!(part2(&parse_input(TEST_DATA)), 1068781)
    }
}