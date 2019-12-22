use intcode::Program;

pub fn part1(file_data: &str)
{
    let mut prog: Program = file_data.parse().unwrap();
    prog.extend_tape(1000);
    prog.push_input(1);

    println!("Day 9 part 1: {}", prog.next().unwrap());
}

pub fn part2(file_data: &str)
{
    let mut prog: Program = file_data.parse().unwrap();
    prog.extend_tape(1000);
    prog.push_input(2);

    println!("Day 9 part 2: {}", prog.next().unwrap());
}