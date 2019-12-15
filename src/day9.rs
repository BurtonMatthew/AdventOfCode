use intcode::Program;

pub fn part1(file_data: &str)
{
    let mut prog_data : Vec<i64> = file_data.split(",")
                                .map(|num| num.parse::<i64>())
                                .filter(|num| num.is_ok())
                                .map(|num| num.unwrap())
                                .collect();
    prog_data.reserve(1000);
    for _ in 0..1000
    {
        prog_data.push(0);
    }

    let mut prog = Program::from_tape(prog_data);
    prog.push_input(1);

    println!("Day 9 part 1: {}", prog.next().unwrap());
    
}

pub fn part2(file_data: &str)
{
    let mut prog_data : Vec<i64> = file_data.split(",")
                                .map(|num| num.parse::<i64>())
                                .filter(|num| num.is_ok())
                                .map(|num| num.unwrap())
                                .collect();

    prog_data.reserve(1000);
    for _ in 0..1000
    {
        prog_data.push(0);
    }

    let mut prog = Program::from_tape(prog_data);
    prog.push_input(2);

    println!("Day 9 part 2: {}", prog.next().unwrap());
}