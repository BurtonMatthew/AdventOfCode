use std::fs::File;
use std::io::{prelude::*, BufReader};

pub fn part1()
{
    let file = File::open("input/day1.txt").expect("Couldn't find day1 input");
    let buf = BufReader::new(file);
    println!("Day 1 Part 1: {}",
        buf.lines()
            .map(|line| fuel_requirement(line.unwrap().parse::<u32>().unwrap()))
            .sum::<u32>());
}

pub fn part2()
{
    let file = File::open("input/day1.txt").expect("Couldn't find day1 input");
    let buf = BufReader::new(file);
    println!("Day 1 Part 2: {}",
        buf.lines()
            .map(|line| wetmass_requirement(line.unwrap().parse::<u32>().unwrap()))
            .sum::<u32>());
}

fn fuel_requirement(mass: u32) -> u32
{
    (mass / 3).saturating_sub(2)
}

#[test]
fn test_fuel_requirement()
{
    assert_eq!(fuel_requirement(12), 2);
    assert_eq!(fuel_requirement(14), 2);
    assert_eq!(fuel_requirement(1969), 654);
    assert_eq!(fuel_requirement(100756), 33583);
}

fn wetmass_requirement(mass: u32) -> u32
{
    let mut total_fuel = 0;
    let mut cur_mass = mass;
    while cur_mass > 0
    {
        cur_mass = fuel_requirement(cur_mass);
        total_fuel += cur_mass;
    }
    total_fuel
}

#[test]
fn test_wetmass_requirement()
{
    assert_eq!(wetmass_requirement(14), 2);
    assert_eq!(wetmass_requirement(1969), 966);
    assert_eq!(wetmass_requirement(100756), 50346);
}