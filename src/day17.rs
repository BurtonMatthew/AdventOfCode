use intcode::Program;

pub fn part1(file_data: &str)
{
    let mut prog_data : Vec<i64> = file_data.split(",")
                                .map(|num| num.parse::<i64>())
                                .filter(|num| num.is_ok())
                                .map(|num| num.unwrap())
                                .collect();
    prog_data.reserve(1000);
    for _ in 0..10000
    {
        prog_data.push(0);
    }

    let program = Program::from_tape(prog_data);
    let map = program.map(|i| char::from(i as u8)).collect::<Vec<_>>().split(|c| *c == '\n').map(|l| l.to_vec()).filter(|v| v.len() > 0).collect::<Vec<Vec<_>>>();
    let width = map[0].len();
    let height = map.len();

    let mut sum_alignment = 0;
    for x in 1..width-2
    {
        for y in 1..height-2
        {
            if map[y][x] == '#' 
                && map[y-1][x] == '#'
                && map[y+1][x] == '#'
                && map[y][x-1] == '#'
                && map[y][x+1] == '#'
            {
                sum_alignment += x * y;
            }
        }
    }


    println!("Day 17 part 1: {}", sum_alignment);
}

pub fn part2(file_data: &str)
{

    let mut prog_data : Vec<i64> = file_data.split(",")
                                .map(|num| num.parse::<i64>())
                                .filter(|num| num.is_ok())
                                .map(|num| num.unwrap())
                                .collect();
    prog_data.reserve(1000);
    for _ in 0..10000
    {
        prog_data.push(0);
    }
    prog_data[0] = 2;
    let mut program = Program::from_tape(prog_data);

    // Hand-solved solution: algorithm is going to be follow path to end before turning, then do a RLE based compression algorithm
    "A,B,A,C,A,B,A,C,B,C\nR,4,L,12,L,8,R,4\nL,8,R,10,R,10,R,6\nR,4,R,10,L,12\nn\n".chars().for_each(|c| program.push_input(c as i64));

    println!("Day 17 part 2: {}", program.last().unwrap());
}

/*
............................###########......
............................#.........#......
............................#.........#......
............................#.........#......
............................#.........#......
............................#.........#......
............................#.....#####......
............................#.....#..........
..............#############.#.....#..........
..............#...........#.#.....#..........
..............#...#####...#.#########........
..............#...#...#...#.......#.#........
..............#...#...#...#############......
..............#...#...#...........#.#.#......
#######.....###########...........#.#########
#.....#.....#.#...#...............#...#.....#
#.....#...#####...#...............#...#.....#
#.....#...#.#.....#...............#...#.....#
#.....#############...............#########.#
#.........#.#.........................#...#.#
#.........#.#.........................#...#.#
#.........#.#.........................#...#.#
#.........#.#.........................#####.#
#.........#.#...............................#
###########.#...............................#
............#...............................#
............#########...................^####
....................#........................
....................#........................
....................#........................
..........#####.....#........................
..........#...#.....#........................
..........#...#.....#........................
..........#...#.....#........................
..........#...#.....#........................
..........#...#.....#........................
..........###########........................
..............#..............................
..............#..............................
..............#..............................
..............#############..................
*/

/*
A B A C A B A C B C
R 4 L 12 L 8 R 4
L 8 R 10 R 10 R 6
R 4 R 10 L 12
*/