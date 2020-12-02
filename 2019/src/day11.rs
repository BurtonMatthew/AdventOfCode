use intcode::Program;
use std::collections::HashMap;

const NORTH : i32 = 0;
const EAST : i32 = 1;
const SOUTH : i32 = 2;
const WEST : i32 = 3;

pub fn part1(file_data: &str)
{
    let mut robot : Program = file_data.parse().unwrap();
    robot.extend_tape(1000);

    let mut robot_pos = (0,0);
    let mut robot_dir = NORTH;
    let mut panels = HashMap::new();
    robot.push_input(0);

    while let Some(paint) = robot.next()
    {
        if let Some(color) = panels.get_mut(&robot_pos)
        {
            *color = paint;
        }
        else
        {
            panels.insert(robot_pos, paint);
        }

        if let Some(dir) = robot.next()
        {
            robot_dir += if dir == 0 { -1 } else { 1 };
            if robot_dir < 0 { robot_dir += 4; }
            robot_dir %= 4;
            let robo_move = match robot_dir
                        {
                            NORTH => (0,1),
                            WEST => (-1,0),
                            EAST => (1,0),
                            SOUTH => (0,-1),
                            _ => panic!("Unexpected Dir"),
                        };
            robot_pos.0 += robo_move.0;
            robot_pos.1 += robo_move.1;

            robot.push_input(*panels.get(&robot_pos).unwrap_or(&0));
        }
        else
        {
            break;
        }
    }

    println!("Day 11 part 1: {}", panels.len());
}

pub fn part2(file_data: &str)
{
    let mut robot : Program = file_data.parse().unwrap();
    robot.extend_tape(1000);

    let mut robot_pos = (0,0);
    let mut robot_dir = NORTH;
    let mut panels = HashMap::new();
    panels.insert((0,0), 1);
    robot.push_input(1);

    while let Some(paint) = robot.next()
    {
        if let Some(color) = panels.get_mut(&robot_pos)
        {
            *color = paint;
        }
        else
        {
            panels.insert(robot_pos, paint);
        }

        if let Some(dir) = robot.next()
        {
            robot_dir += if dir == 0 { -1 } else { 1 };
            if robot_dir < 0 { robot_dir += 4; }
            robot_dir %= 4;
            let robo_move = match robot_dir
                        {
                            NORTH => (0,-1),
                            WEST => (-1,0),
                            EAST => (1,0),
                            SOUTH => (0,1),
                            _ => panic!("Unexpected Dir"),
                        };
            robot_pos.0 += robo_move.0;
            robot_pos.1 += robo_move.1;
            
            robot.push_input(*panels.get(&robot_pos).unwrap_or(&0));
        }
        else
        {
            break;
        }
    }

    let bounds = panels.keys().fold((i32::max_value(), i32::min_value(), i32::max_value(), i32::min_value()) , |(min_x, max_x, min_y, max_y), (x,y)|
    {
        (i32::min(min_x, *x), i32::max(max_x, *x), i32::min(min_y, *y), i32::max(max_y, *y))
    });

    println!("Day 11 part 2:");

    for y in bounds.2..bounds.3+1
    {
        println!("{}", (bounds.0..bounds.1+1).map(|x| if *panels.get(&(x,y)).unwrap_or(&0) == 0 {' '} else {'#'}).collect::<String>());
    } 
}