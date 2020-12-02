use intcode::Program;
//use std::io::stdin; for manual play

pub fn part(file_data: &str)
{
    let mut prog : Program = file_data.parse().unwrap();
    prog.extend_tape(1000);

    // Hardcoded solution
    let solution = 
    "south
    south
    west
    north
    north
    take tambourine
    south
    south
    east
    south
    take fixed point
    south
    west
    west
    south
    take easter egg
    north
    east
    east
    north
    north
    north
    west
    west
    west
    take space heater
    west
    west";

    for line in solution.lines()
    {
        line.trim().chars().map(|c| c as i64).for_each(|i| prog.push_input(i));
        prog.push_input('\n' as i64);
    }

    let result = prog.map(|c| c as u8 as char)
                        .collect::<String>()
                        .split_whitespace()
                        .map(|word| word.parse::<i64>())
                        .find_map(|x| x.ok())
                        .unwrap();

    println!("Day 25: {}", result);

    // Manual playthough
    /*
    loop
    {
        println!("{}", (&mut prog).map(|c| c as u8 as char).collect::<String>());
        let mut input = String::new();
        stdin().read_line(&mut input).expect("Failed! >:C");
        input.trim().chars().map(|c| c as i64).for_each(|i| prog.push_input(i));
        prog.push_input('\n' as i64);
    }
    */
}