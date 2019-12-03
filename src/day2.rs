use std::fs::File;
use std::io::{prelude::*};

pub fn part1()
{
    let mut file = File::open("input/day2.txt").expect("Couldn't find day2 input");
    let mut file_data = String::new();
    file.read_to_string(&mut file_data).expect("Unable to read file");

    println!("Day 2 Part 1: {}",
        run_program(set_inputs(12, 2, file_data.split(",")
                                                    .map(|num| num.parse::<i32>())
                                                    .filter(|num| num.is_ok())
                                                    .map(|num| num.unwrap())
                                                    .collect())).unwrap()[0]);

}

pub fn part2()
{
    let mut file = File::open("input/day2.txt").expect("Couldn't find day2 input");
    let mut file_data = String::new();
    file.read_to_string(&mut file_data).expect("Unable to read file");
    let program = file_data.split(",")
                            .map(|num| num.parse::<i32>())
                            .filter(|num| num.is_ok())
                            .map(|num| num.unwrap())
                            .collect::<Vec<i32>>();

    for noun in 1..99
    {
        for verb in 1..99
        {
            if run_program(set_inputs(noun, verb, program.clone())).unwrap()[0] == 19690720
            {
                println!("Day 2 Part 2: {}", noun * 100 + verb);
                return;
            }
        }
    }
}

fn set_inputs(noun: i32, verb: i32, in_tape: Vec<i32>) -> Vec<i32>
{
    let mut out_tape = in_tape;
    out_tape[1] = noun;
    out_tape[2] = verb;
    out_tape
}

fn run_program(input_tape: Vec<i32>) -> Result<Vec<i32>, String>
{
    let mut out_tape = input_tape;
    let mut pc = 0;
    let mut success = true;
    loop
    {
        let instruction = out_tape[pc];
        if instruction == 1
        {
            let write_index = out_tape[pc+3] as usize;
            out_tape[write_index] = out_tape[out_tape[pc+1] as usize] + out_tape[out_tape[pc+2] as usize];
        }
        else if instruction == 2
        {
            let write_index = out_tape[pc+3] as usize;
            out_tape[write_index] = out_tape[out_tape[pc+1] as usize] * out_tape[out_tape[pc+2] as usize];
        }
        else if instruction == 99
        {
            break;
        }
        else
        {
            success = false;
            break;
        }

        pc += 4;
    }
    match success
    {
        true => Ok(out_tape),
        false => Err(String::from("Invalid Opcode Found"))
    }
}

#[test]
fn test_run_program()
{
    assert_eq!(run_program(vec![1,0,0,0,99]).unwrap(), vec![2,0,0,0,99]);
    assert_eq!(run_program(vec![2,3,0,3,99]).unwrap(), vec![2,3,0,6,99]);
    assert_eq!(run_program(vec![2,4,4,5,99,0]).unwrap(), vec![2,4,4,5,99,9801]);
    assert_eq!(run_program(vec![1,1,1,4,99,5,6,0,99]).unwrap(), vec![30,1,1,4,2,5,6,0,99]);
}