use std::fs::File;
use std::io::{prelude::*};
use intcode::Program;
use itertools::Itertools;

pub fn part1()
{
    let mut file = File::open("input/day7.txt").expect("Couldn't find day5 input");
    let mut file_data = String::new();
    file.read_to_string(&mut file_data).expect("Unable to read file");

    let prog_data : Vec<i32> = file_data.split(",")
                                .map(|num| num.parse::<i32>())
                                .filter(|num| num.is_ok())
                                .map(|num| num.unwrap())
                                .collect();

    let amplifier = |phase, signal| 
                    { 
                        let mut prog = Program::from_tape(prog_data.clone());
                        prog.push_input(phase);
                        prog.push_input(signal);
                        prog.next().unwrap()
                    };

    let result = (0..5).permutations(5)
        .into_iter()
        .map(|v| 
            { 
                let mut signal = 0;
                for phase in v
                {
                    signal = amplifier(phase, signal);
                }
                signal
            }).max().unwrap();

    println!("Day 7 Part 1: {}", result);
}

pub fn part2()
{
    let mut file = File::open("input/day7.txt").expect("Couldn't find day5 input");
    let mut file_data = String::new();
    file.read_to_string(&mut file_data).expect("Unable to read file");

    let prog_data : Vec<i32> = file_data.split(",")
                                .map(|num| num.parse::<i32>())
                                .filter(|num| num.is_ok())
                                .map(|num| num.unwrap())
                                .collect();
    const NUM_AMPS : usize = 5;

    let result = (5..10).permutations(NUM_AMPS)
        .into_iter()
        .map(|perm|
    {
        let mut amps = vec![Program::from_tape(prog_data.clone()); NUM_AMPS];
        for i in 0..NUM_AMPS
        {
            amps[i].push_input(perm[i]);
        }
        amps[0].push_input(0);

        let mut latest_output = 0;
        let mut next_amp = 0;
        while let Some(cur_output) = amps[next_amp].next()
        {
            if next_amp == NUM_AMPS -1
            {
                next_amp = 0;
                latest_output = cur_output;
            }
            else
            {
                next_amp += 1;
            }

            amps[next_amp].push_input(cur_output);
        }
        latest_output
    }).max().unwrap();

    println!("Day 7 Part 2: {}", result);
}