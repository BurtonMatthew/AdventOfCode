use vec2::Vec2;

#[aoc_generator(day3)]
pub fn parse_input(buf : &str) -> Vec2<char>
{
    buf.parse().unwrap()
}

#[aoc(day3, part1)]
pub fn part1(input : &Vec2<char>) -> usize
{
    slopes(&input, 3, 1)
}

#[aoc(day3, part2)]
pub fn part2(input : &Vec2<char>) -> usize
{
    [(1,1), (3,1), (5,1), (7,1), (1,2)].iter().fold(1, |prod, (r,d)| prod * slopes(&input, *r, *d))
}

fn slopes(i: &Vec2<char>, r: usize, d: usize) -> usize
{
    let mut x = 0;
    let mut y = 0;
    let mut c = 0;
    let w = i.width();
    let h = i.height();

    while y < h
    {
        if i[y][x] == '#' { c += 1; }
        x = (x+r) % w;
        y += d;
    }
    c
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#";

    #[test]
    pub fn slopes_test() 
    {
        assert_eq!(slopes(&parse_input(TEST_DATA), 3, 1), 7)
    }
}