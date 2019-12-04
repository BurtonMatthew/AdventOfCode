use std::fs::File;
use std::io::{prelude::*};

pub fn part1()
{
    let mut file = File::open("input/day3.txt").expect("Couldn't find day3 input");
    let mut file_data = String::new();
    file.read_to_string(&mut file_data).expect("Unable to read file");
    let wires : Vec<Vec<Line>> = file_data.lines().map(|l| parse_wire(&l)).collect();

    println!("Day 3 Part 1: {}", closest_intersection_manhattan(&find_intersections(&wires[0], &wires[1])));
}

pub fn part2()
{
    let mut file = File::open("input/day3.txt").expect("Couldn't find day3 input");
    let mut file_data = String::new();
    file.read_to_string(&mut file_data).expect("Unable to read file");
    let wires : Vec<Vec<Line>> = file_data.lines().map(|l| parse_wire(&l)).collect();

    println!("Day 3 Part 2: {}", closest_intersection_wire(&wires[0], &wires[1], &find_intersections(&wires[0], &wires[1])));
}

fn parse_wire(string: &str) -> Vec<Line>
{
    let tokens : Vec<&str> = string.split(",").collect();
    let mut x = 0;
    let mut y = 0;
    let mut result = Vec::with_capacity(tokens.len());

    for token in tokens
    {
        let direction = &token[..1];
        let distance = &token[1..].parse::<i32>().unwrap();
        result.push(
            match direction
            {
                "U" => { let new = Line{x1: x, y1: y, x2: x, y2: y+distance, startx: x, starty: y}; y += distance; new }
                "D" => { let new = Line{x1: x, y1: y-distance, x2: x, y2: y, startx: x, starty: y}; y -= distance; new }
                "L" => { let new = Line{x1: x-distance, y1: y, x2: x, y2: y, startx: x, starty: y}; x -= distance; new }
                "R" => { let new = Line{x1: x, y1: y, x2: x+distance, y2: y, startx: x, starty: y}; x += distance; new }
                _ => Line{x1: 0, y1: 0, x2: 0, y2: 0, startx: 0, starty: 0}
            }
        );
    }
    result
}

fn find_intersections(wires_one: &Vec<Line>, wires_two: &Vec<Line>) -> Vec<(i32,i32)>
{
    let mut result : Vec<(i32,i32)> = Vec::new();

    for left in wires_one
    {
        for right in wires_two
        {
            if left.intersects(*right)
            {
                let mut x = 0;
                let mut y = 0;
                if left.x1 == left.x2 { x = left.x1; }
                if left.y1 == left.y2 { y = left.y1; }
                if right.x1 == right.x2 { x = right.x1; }
                if right.y1 == right.y2 { y = right.y1; }

                result.push((x,y));
            }
        }
    }

    result
}

fn closest_intersection_manhattan(intersections: &Vec<(i32,i32)>) -> i32
{
    intersections.iter()
        .map(|(x,y)| x.abs() + y.abs())
        .filter(|n| *n > 0)
        .min()
        .unwrap()
}

#[test]
fn test_solve_part_1()
{
    let test1_left = parse_wire("R75,D30,R83,U83,L12,D49,R71,U7,L72");
    let test1_right = parse_wire("U62,R66,U55,R34,D71,R55,D58,R83");
    assert_eq!(closest_intersection_manhattan(&find_intersections(&test1_left, &test1_right)), 159);

    let test2_left = parse_wire("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51");
    let test2_right = parse_wire("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7");
    assert_eq!(closest_intersection_manhattan(&find_intersections(&test2_left, &test2_right)), 135);
}

fn closest_intersection_wire(wires_one: &Vec<Line>, wires_two: &Vec<Line>, intersections: &Vec<(i32,i32)>) -> i32
{
    let mut result = i32::max_value();

    // Not in love with this algorithm
    for intersection in intersections
    {
        let line_int = Line {x1: intersection.0, x2: intersection.0, y1: intersection.1, y2: intersection.1, startx: intersection.0, starty: intersection.1 };
        let mut dist = 0;

        for wire in wires_one
        {
            if wire.intersects(line_int)
            {
                dist += (wire.startx - line_int.x1).abs() + (wire.starty - line_int.y1).abs();
                break;
            }
            else
            {
                dist += wire.x2 - wire.x1 + wire.y2 - wire.y1;
            }
        }

        for wire in wires_two
        {
            if wire.intersects(line_int)
            {
                dist += (wire.startx - line_int.x1).abs() + (wire.starty - line_int.y1).abs();
                break;
            }
            else
            {
                dist += wire.x2 - wire.x1 + wire.y2 - wire.y1;
            }
        }

        if dist > 0
        {
            result = i32::min(result, dist);
        }
    }

    result
}

#[test]
fn test_solve_part_2()
{
    let test1_left = parse_wire("R75,D30,R83,U83,L12,D49,R71,U7,L72");
    let test1_right = parse_wire("U62,R66,U55,R34,D71,R55,D58,R83");
    assert_eq!(closest_intersection_wire(&test1_left, &test1_right, &find_intersections(&test1_left, &test1_right)), 610);

    let test2_left = parse_wire("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51");
    let test2_right = parse_wire("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7");
    assert_eq!(closest_intersection_wire(&test2_left, &test2_right, &find_intersections(&test2_left, &test2_right)), 410);
}


#[derive(Copy, Clone)]
struct Line
{
    x1: i32,
    y1: i32,
    x2: i32,
    y2: i32,
    startx: i32,
    starty: i32,
}

impl Line
{
    fn intersects(&self, other: Line) -> bool
    {
        self.x1 <= other.x2 && self.x2 >= other.x1 && self.y1 <= other.y2 && self.y2 >= other.y1
    }
}

