use vec2::Vec2;

// 2D character grid
type InputType = Vec2<char>;
#[aoc_generator(day17)]
pub fn parse_input(buf : &str) -> InputType
{
    buf.parse().unwrap()
}

#[aoc(day17, part1)]
pub fn part1(input : &InputType) -> usize
{

    const ITERATIONS:usize = 6;
    const PADDING: usize = 4;
    let mut cubes = vec![vec![vec![false; input.width() + ITERATIONS + ITERATIONS + PADDING]; input.height() + ITERATIONS + ITERATIONS + PADDING]; ITERATIONS + ITERATIONS + PADDING];
    
    for (y, row) in input.rows().enumerate()
    {
        for (x, value) in row.iter().enumerate()
        {
            cubes[ITERATIONS+PADDING/2][y+ITERATIONS+PADDING/2][x+ITERATIONS+PADDING/2] = *value == '#';
        }
    }

    for _ in 0..ITERATIONS
    {
        let mut new_cubes = cubes.clone();

        for z in 1..cubes.len()-1
        {
            for y in 1..cubes[z].len()-1
            {
                for x in 1..cubes[z][y].len()-1
                {
                    let mut active_neighbors = 0;
                    for dz in -1..=1isize
                    {
                        for dy in -1..=1isize
                        {
                            for dx in -1..=1isize
                            {
                                if dx == 0 && dy == 0 && dz == 0
                                {
                                    continue;
                                }
                                else
                                {
                                    if cubes[(z as isize+dz) as usize][(y as isize+dy) as usize][(x as isize+dx) as usize]
                                    {
                                        active_neighbors += 1;
                                    }
                                }
                            }
                        }
                    }

                    if cubes[z][y][x] && (active_neighbors == 2 || active_neighbors == 3) {}
                    else { new_cubes[z][y][x] = false; }

                    if !cubes[z][y][x] && active_neighbors == 3 { new_cubes[z][y][x] = true; }
                }
            }
        }
        cubes = new_cubes.clone();
    }

    let mut active = 0;
    for z in 0..cubes.len()
    {
        for y in 0..cubes[z].len()
        {
            for x in 0..cubes[z][y].len()
            {
                if cubes[z][y][x]
                {
                    active += 1;
                }
            }
        }
    }
    active
}

#[aoc(day17, part2)]
pub fn part2(input : &InputType) -> usize
{

    const ITERATIONS:usize = 6;
    const PADDING: usize = 4;
    let mut cubes = vec![vec![vec![vec![false; input.width() + ITERATIONS + ITERATIONS + PADDING]; input.height() + ITERATIONS + ITERATIONS + PADDING]; ITERATIONS + ITERATIONS + PADDING]; ITERATIONS + ITERATIONS + PADDING];
    
    for (y, row) in input.rows().enumerate()
    {
        for (x, value) in row.iter().enumerate()
        {
            cubes[ITERATIONS+PADDING/2][ITERATIONS+PADDING/2][y+ITERATIONS+PADDING/2][x+ITERATIONS+PADDING/2] = *value == '#';
        }
    }

    for _ in 0..ITERATIONS
    {
        let mut new_cubes = cubes.clone();
        for w in 1..cubes.len()-1
        {
            for z in 1..cubes[w].len()-1
            {
                for y in 1..cubes[w][z].len()-1
                {
                    for x in 1..cubes[w][z][y].len()-1
                    {
                        let mut active_neighbors = 0;
                        for dw in -1..=1isize
                        {
                            for dz in -1..=1isize
                            {
                                for dy in -1..=1isize
                                {
                                    for dx in -1..=1isize
                                    {
                                        if dx == 0 && dy == 0 && dz == 0 && dw == 0
                                        {
                                            continue;
                                        }
                                        else
                                        {
                                            if cubes[(w as isize+dw) as usize][(z as isize+dz) as usize][(y as isize+dy) as usize][(x as isize+dx) as usize]
                                            {
                                                active_neighbors += 1;
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        if cubes[w][z][y][x] && (active_neighbors == 2 || active_neighbors == 3) {}
                        else { new_cubes[w][z][y][x] = false; }

                        if !cubes[w][z][y][x] && active_neighbors == 3 { new_cubes[w][z][y][x] = true; }
                    }
                }
            }
        }
        cubes = new_cubes.clone();
    }

    let mut active = 0;
    for w in 0..cubes.len()
    {
        for z in 0..cubes[w].len()
        {
            for y in 0..cubes[w][z].len()
            {
                for x in 0..cubes[w][z][y].len()
                {
                    if cubes[w][z][y][x]
                    {
                        active += 1;
                    }
                }
            }
        }
    }
    active
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
".#.
..#
###";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(&parse_input(TEST_DATA)), 112)
    }

    #[test]
    pub fn part2_test() 
    {
        assert_eq!(part2(&parse_input(TEST_DATA)), 848)
    }
}