#![allow(incomplete_features)]
#![feature(const_generics)]

use std::time::Instant;
use std::fs;

mod intcode;
mod vec2;
mod modular;
mod day1;
mod day2;
mod day3;
mod day4;
mod day5;
mod day6;
mod day7;
mod day8;
mod day9;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;
mod day15;
mod day16;
mod day17;
mod day18;
mod day19;
mod day20;
mod day21;
mod day22;
mod day23;
mod day24;
mod day25;
extern crate itertools;
extern crate num;
extern crate pathfinding;

fn main() 
{
    let now = Instant::now();
    let files = (1..26).map(|i| fs::read_to_string(format!("input/day{}.txt", i)).unwrap_or_default()).collect::<Vec<String>>();
    day1::part1(files[0].trim());
    day1::part2(files[0].trim());
    day2::part1(files[1].trim());
    day2::part2(files[1].trim());
    day3::part1(files[2].trim());
    day3::part2(files[2].trim());
    day4::part1();
    day4::part2();
    day5::part1(files[4].trim());
    day5::part2(files[4].trim());
    day6::part1(files[5].trim());
    day6::part2(files[5].trim());
    day7::part1(files[6].trim());
    day7::part2(files[6].trim());
    day8::part1(files[7].trim());
    day8::part2(files[7].trim());
    day9::part1(files[8].trim());
    day9::part2(files[8].trim());
    let station = day10::part1(files[9].trim());
    day10::part2(files[9].trim(), station);
    day11::part1(files[10].trim());
    day11::part2(files[10].trim());
    day12::part1(files[11].trim());
    day12::part2(files[11].trim());
    day13::part1(files[12].trim());
    day13::part2(files[12].trim());
    day14::part1(files[13].trim());
    day14::part2(files[13].trim());
    let mut map = day15::part1(files[14].trim());
    day15::part2(&mut map);
    day16::part1(files[15].trim());
    day16::part2(files[15].trim());
    day17::part1(files[16].trim());
    day17::part2(files[16].trim());
    day18::part1(files[17].trim());
    day18::part2(files[17].trim());
    day19::part1(files[18].trim());
    day19::part2(files[18].trim());
    day20::part1(&files[19]);
    day20::part2(&files[19]);
    day21::part1(files[20].trim());
    day21::part2(files[20].trim());
    day22::part1(files[21].trim());
    day22::part2(files[21].trim());
    day23::part1(files[22].trim());
    day23::part2(files[22].trim());
    day24::part1(files[23].trim());
    day24::part2(files[23].trim());
    day25::part(files[24].trim());
    println!("Total time: {}ms", now.elapsed().as_millis());
}