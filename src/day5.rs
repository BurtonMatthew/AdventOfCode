use intcode::Program;

pub fn part1(file_data: &str)
{
    let mut program = Program::from_tape(file_data.split(",")
                                                .map(|num| num.parse::<i64>())
                                                .filter(|num| num.is_ok())
                                                .map(|num| num.unwrap())
                                                .collect());
    program.push_input(1);

    println!("Day 5 Part 1: {}", program.filter(|n| *n > 0).collect::<Vec<i64>>()[0]);

}

pub fn part2(file_data: &str)
{
    let mut program = Program::from_tape(file_data.split(",")
                                                .map(|num| num.parse::<i64>())
                                                .filter(|num| num.is_ok())
                                                .map(|num| num.unwrap())
                                                .collect());
    program.push_input(5);

    println!("Day 5 Part 2: {}", program.filter(|n| *n > 0).collect::<Vec<i64>>()[0]);
}