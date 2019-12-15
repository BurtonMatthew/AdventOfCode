use std::collections::HashMap;
use intcode::Program;

const NORTH : i64 = 1;
const SOUTH : i64 = 2;
const WEST : i64 = 3;
const EAST : i64 = 4;

const WALL : i64 = 0;
const OXYGEN : i64 = 2;

pub fn part1(file_data: &str) -> HashMap<(i32,i32),i32>
{
    let mut prog_data : Vec<i64> = file_data.split(",")
                                .map(|num| num.parse::<i64>())
                                .filter(|num| num.is_ok())
                                .map(|num| num.unwrap())
                                .collect();
    prog_data.reserve(1000);
    for _ in 0..1000
    {
        prog_data.push(0);
    }

    let mut map = HashMap::new();
    println!("Day 15 part 1: {}", fastest_oxygen(&mut Program::from_tape(prog_data), &mut map, (0,0), 0));
    map
}

pub fn part2(map: &mut HashMap<(i32,i32),i32>)
{
    let mut start_pos = (0,0);
    for (pos,val) in map.iter_mut()
    {
        if *val == -1 { start_pos = *pos;}
        else { *val = i32::max_value(); }
    }

    flood_fill_oxygen(map, start_pos, 0);
    println!("Day 15 part 2: {}", map.values().max().unwrap());
}

fn step_position(pos : (i32,i32), dir: i64) -> (i32,i32)
{
    match dir
    {
        NORTH => (pos.0, pos.1-1),
        SOUTH => (pos.0, pos.1+1),
        WEST => (pos.0-1, pos.1),
        EAST => (pos.0+1, pos.1),
        _ => panic!("Invalid Dir")
    }
}

fn fastest_oxygen(robot: &mut Program, map: &mut HashMap<(i32,i32), i32>, pos: (i32, i32), depth: i32) -> i32
{
    map.insert(pos, depth);

    let mut dir_results = [0;4];

    fn opposite_direction(dir: i64) -> i64
    {
        match dir
        {
            NORTH => SOUTH,
            SOUTH => NORTH,
            WEST => EAST,
            EAST => WEST,
            _ => panic!("Invalid Dir")
        }
    }

    for dir in 1..5
    {
        let opp_dir = opposite_direction(dir);
        let new_pos = step_position(pos, dir);
        if depth+1 < *map.get(&new_pos).unwrap_or(&i32::max_value())
        {
            robot.push_input(dir);
            let move_result = robot.next().unwrap();
            if move_result != WALL
            {
                if move_result == OXYGEN
                {
                    robot.push_input(opp_dir);
                    robot.next();
                    map.insert(new_pos, -1);
                    return depth+1;
                }

                dir_results[(dir-1) as usize] = fastest_oxygen(robot, map, new_pos, depth+1);
                robot.push_input(opp_dir);
                robot.next();
            }
            else { dir_results[(dir-1) as usize] = i32::max_value(); }
        }
        else { dir_results[(dir-1) as usize] = i32::max_value(); }
    }

    *dir_results.iter().min().unwrap()
}

fn flood_fill_oxygen(map: &mut HashMap<(i32,i32), i32>, pos: (i32, i32), depth: i32)
{
    map.insert(pos, depth);

    for dir in 1..5
    {
        let new_pos = step_position(pos, dir);
        if let Some(dist) = map.get_mut(&new_pos)
        {
            if depth + 1 < *dist
            {
                flood_fill_oxygen(map, new_pos, depth+1);
            }
        }
    }
}