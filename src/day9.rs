use std::fs::File;
use std::io::{prelude::*};
use intcode::Program;

pub fn part1()
{
    let mut file = File::open("input/day9.txt").expect("Couldn't find day9 input");
    let mut file_data = String::new();
    file.read_to_string(&mut file_data).expect("Unable to read file");

    let prog_data : Vec<i64> = file_data.split(",")
                                .map(|num| num.parse::<i64>())
                                .filter(|num| num.is_ok())
                                .map(|num| num.unwrap())
                                .collect();

    let mut prog = Program::from_tape(prog_data.clone());
    prog.push_input(1);

    println!("Day 9 part 1: {}", prog.next().unwrap());
    
}

pub fn part2()
{
    let mut file = File::open("input/day9.txt").expect("Couldn't find day9 input");
    let mut file_data = String::new();
    file.read_to_string(&mut file_data).expect("Unable to read file");

    let prog_data : Vec<i64> = file_data.split(",")
                                .map(|num| num.parse::<i64>())
                                .filter(|num| num.is_ok())
                                .map(|num| num.unwrap())
                                .collect();

    let mut prog = Program::from_tape(prog_data.clone());
    prog.push_input(2);

    println!("Day 9 part 2: {}", prog.next().unwrap());
}