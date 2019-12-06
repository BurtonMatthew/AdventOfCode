use std::fs::File;
use std::io::{prelude::*};

pub fn part1()
{
    let mut file = File::open("input/day5.txt").expect("Couldn't find day2 input");
    let mut file_data = String::new();
    file.read_to_string(&mut file_data).expect("Unable to read file");

    let istream : Vec<i32> = vec![1];
    let ostream = run_program(istream, file_data.split(",")
                                .map(|num| num.parse::<i32>())
                                .filter(|num| num.is_ok())
                                .map(|num| num.unwrap())
                                .collect());

    println!("Day 5 Part 1: {}", ostream.into_iter().filter(|n| *n > 0).collect::<Vec<i32>>()[0]);

}

pub fn part2()
{
    let mut file = File::open("input/day5.txt").expect("Couldn't find day2 input");
    let mut file_data = String::new();
    file.read_to_string(&mut file_data).expect("Unable to read file");

    let istream : Vec<i32> = vec![5];
    let ostream = run_program(istream, file_data.split(",")
                                .map(|num| num.parse::<i32>())
                                .filter(|num| num.is_ok())
                                .map(|num| num.unwrap())
                                .collect());

    println!("Day 5 Part 2: {}", ostream[0]);
}

enum Op
{
    Add,
    Mul,
    In,
    Out,
    Tjmp,
    Fjmp,
    LessThan,
    Equals,
    Halt
}

fn to_op(code: i32) -> Op
{
    match code
    {
        1 => Op::Add,
        2 => Op::Mul,
        3 => Op::In,
        4 => Op::Out,
        5 => Op::Tjmp,
        6 => Op::Fjmp,
        7 => Op::LessThan,
        8 => Op::Equals,
        99 => Op::Halt,
        _ => panic!("Invalid Opcode")
    }
}

fn num_params(op: &Op) -> usize
{
    match op
    {
        Op::Add => 2,
        Op::Mul => 2,
        Op::In => 0,
        Op::Out => 1,
        Op::Tjmp => 2,
        Op::Fjmp => 2,
        Op::LessThan => 2,
        Op::Equals => 2,
        Op::Halt => 0,
    }
}

fn has_output(op: &Op) -> bool
{
    match op
    {
        Op::Add => true,
        Op::Mul => true,
        Op::In => true,
        Op::Out => false,
        Op::Tjmp => false,
        Op::Fjmp => false,
        Op::LessThan => true,
        Op::Equals => true,
        Op::Halt => false,
    }
}

fn run_program(istream: Vec<i32>, mut tape: Vec<i32>) -> Vec<i32>
{
    let mut pc = 0;
    let mut ostream = Vec::new();
    let mut istreamiter = istream.iter();
    loop
    {
        // read opcode
        let mut opcode = tape[pc];
        let op = to_op(opcode % 100);
        opcode /= 100;
        pc += 1;

        // read params
        let mut params : [i32; 2] = [0;2];
        for i in 0..num_params(&op)
        {
            params[i] = if opcode % 10 == 0 { tape[tape[pc] as usize] } else { tape[pc] };
            opcode /= 10;
            pc += 1;
        }

        // read destination
        let out_index = if has_output(&op) 
                        { 
                            let index = tape[pc];
                            pc += 1;
                            index
                        } 
                        else { 0 } as usize;
       
        match op
        {
            Op::Add => tape[out_index] = params[0] + params[1],
            Op::Mul => tape[out_index] = params[0] * params[1],
            Op::In => tape[out_index] = *istreamiter.next().unwrap(),
            Op::Out => ostream.push(params[0]),
            Op::Tjmp => if params[0] != 0 { pc = params[1] as usize; },
            Op::Fjmp => if params[0] == 0 { pc = params[1] as usize; },
            Op::LessThan => tape[out_index] = if params[0] < params[1] { 1 } else { 0 },
            Op::Equals => tape[out_index] = if params[0] == params[1] { 1 } else { 0 },
            Op::Halt => break,
        }
    }

    ostream
}