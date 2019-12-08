use std::fs::File;
use std::io::{prelude::*};
use intcode;

pub fn part1()
{
    let mut file = File::open("input/day5.txt").expect("Couldn't find day5 input");
    let mut file_data = String::new();
    file.read_to_string(&mut file_data).expect("Unable to read file");

    let mut program = intcode::Program::from_tape(file_data.split(",")
                                                .map(|num| num.parse::<i32>())
                                                .filter(|num| num.is_ok())
                                                .map(|num| num.unwrap())
                                                .collect());
    program.push_input(1);

    println!("Day 5 Part 1: {}", program.filter(|n| *n > 0).collect::<Vec<i32>>()[0]);

}

pub fn part2()
{
    let mut file = File::open("input/day5.txt").expect("Couldn't find day5 input");
    let mut file_data = String::new();
    file.read_to_string(&mut file_data).expect("Unable to read file");

    let mut program = intcode::Program::from_tape(file_data.split(",")
                                                .map(|num| num.parse::<i32>())
                                                .filter(|num| num.is_ok())
                                                .map(|num| num.unwrap())
                                                .collect());
    program.push_input(5);

    println!("Day 5 Part 2: {}", program.filter(|n| *n > 0).collect::<Vec<i32>>()[0]);
}