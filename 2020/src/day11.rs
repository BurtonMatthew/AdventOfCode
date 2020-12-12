use vec2::Vec2;
use std::mem::swap;

// 2D character grid
type InputType = Vec2<char>;
#[aoc_generator(day11)]
pub fn parse_input(buf : &str) -> InputType
{
    buf.parse().unwrap()
}

#[aoc(day11, part1)]
pub fn part1(input : &InputType) -> usize
{
    let mut prev = input.clone();
    let mut next = input.clone();
    let mut modified = true;
    let w = input.width();

    while modified
    {
        modified = false;
        // Todo write a 2d enumerate
        for (i, (elem, neighbors)) in prev.neighbors8_with_padding(&'.').enumerate()
        {
            next[i/w][i%w] = match elem
                {
                    'L' => if neighbors.iter().filter(|&&&c| c == '#').count() == 0 { modified = true; '#' } else { 'L' },
                    '#' => if neighbors.iter().filter(|&&&c| c == '#').count() >= 4 { modified = true; 'L' } else { '#' },
                    _ => *elem
                };
        }
        swap(&mut prev, &mut next);
    }

    prev.iter().filter(|&&c| c == '#').count()
}

#[aoc(day11, part1, part1_old)]
pub fn part1_old(input : &InputType) -> usize
{
    let mut last = input.clone();

    loop
    {
        let next = p1(&last);
        if next == last
        {
            break;
        }
        else
        {
            last = next;
        }
    }

    last.iter().filter(|&c| *c == '#').count()
}

pub fn p1(input : &InputType) -> Vec2<char>
{
    let mut data: Vec<char> = Vec::with_capacity(input.width() * input.height());
    for y in 0..input.height()
    {
        for x in 0..input.width()
        {
            data.push(get_new(input, x, y));
        }
    }

    Vec2::from_vec(data, input.width())
}

pub fn get_new(input : &InputType, x:usize, y:usize) -> char
{
    let w = input.width() -1;
    let h = input.height() -1;
    let mut num_adj_occupied = 0;
    if x > 0 && y > 0 && input[y-1][x-1] == '#' { num_adj_occupied += 1; }
    if x > 0 && y < h && input[y+1][x-1] == '#' { num_adj_occupied += 1; }
    if x < w && y > 0 && input[y-1][x+1] == '#' { num_adj_occupied += 1; }
    if x < w && y < h && input[y+1][x+1] == '#' { num_adj_occupied += 1; }
    if x > 0 && input[y][x-1] == '#' { num_adj_occupied += 1; }
    if x < w && input[y][x+1] == '#' { num_adj_occupied += 1; }
    if y > 0 && input[y-1][x] == '#' { num_adj_occupied += 1; }
    if y < h && input[y+1][x] == '#' { num_adj_occupied += 1; }

    if input[y][x] == 'L' && num_adj_occupied == 0 { '#' }
    else if input[y][x] == '#' && num_adj_occupied >= 4 { 'L' }
    else { input[y][x] }
}

#[aoc(day11, part2)]
pub fn part2(input : &InputType) -> usize
{
    let mut last = input.clone();

    loop
    {
        let next = p2(&last);
        if next == last
        {
            break;
        }
        else
        {
            last = next;
        }
    }

    last.iter().filter(|&c| *c == '#').count()
}

pub fn p2(input : &InputType) -> Vec2<char>
{
    let mut data: Vec<char> = Vec::with_capacity(input.width() * input.height());
    for y in 0..input.height()
    {
        for x in 0..input.width()
        {
            data.push(get_new_2(input, x as i32, y as i32));
        }
    }

    Vec2::from_vec(data, input.width())
}

pub fn get_new_2(input : &InputType, x:i32, y:i32) -> char
{
    let mut num_adj_occupied = 0;
    if is_occ(input, x-1, y-1, -1, -1) { num_adj_occupied += 1; }
    if is_occ(input, x-1, y+1, -1, 1) { num_adj_occupied += 1; }
    if is_occ(input, x+1, y-1, 1, -1) { num_adj_occupied += 1; }
    if is_occ(input, x+1, y+1, 1, 1) { num_adj_occupied += 1; }

    if is_occ(input, x, y-1, 0, -1) { num_adj_occupied += 1; }
    if is_occ(input, x, y+1, 0, 1) { num_adj_occupied += 1; }
    if is_occ(input, x+1, y, 1, 0) { num_adj_occupied += 1; }
    if is_occ(input, x-1, y, -1, 0) { num_adj_occupied += 1; }
    

    if input[y as usize][x as usize] == 'L' && num_adj_occupied == 0 { '#' }
    else if input[y  as usize][x  as usize] == '#' && num_adj_occupied >= 5 { 'L' }
    else { input[y  as usize][x as usize] }
}

pub fn is_occ(input : &InputType, x:i32, y:i32, dx:i32, dy:i32) -> bool
{
    if x < 0 || x as usize >= input.width() || y < 0 || y as usize >= input.height()
    {
        false
    }
    else
    {
        match input[y as usize][x as usize]
        {
            '#' => true,
            'L' => false,
            '.' => is_occ(input, x+dx, y+dy, dx, dy),
            _ => unreachable!()
        }
    }
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
        assert_eq!(part1(&parse_input(TEST_DATA)), 0)
    }

    #[test]
    pub fn part2_test() 
    {
        assert_eq!(part2(&parse_input(TEST_DATA)), 0)
    }
}