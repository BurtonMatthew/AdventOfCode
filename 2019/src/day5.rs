use intcode::Program;

pub fn part1(file_data: &str)
{
    let mut program: Program = file_data.parse().unwrap();
    program.push_input(1);

    println!("Day 5 Part 1: {}", program.filter(|n| *n > 0).next().unwrap());

}

pub fn part2(file_data: &str)
{
    let mut program: Program = file_data.parse().unwrap();
    program.push_input(5);

    println!("Day 5 Part 2: {}", program.filter(|n| *n > 0).next().unwrap());
}