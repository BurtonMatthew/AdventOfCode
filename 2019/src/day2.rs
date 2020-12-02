use intcode::Program;

pub fn part1(file_data: &str)
{
    let mut prog : Program = file_data.parse().unwrap();
    prog[1] = 12;
    prog[2] = 2;
    prog.next();
    println!("Day 2 Part 1: {}", prog[0]);

}

pub fn part2(file_data: &str)
{
    let proto_prog : Program = file_data.parse().unwrap();

    for noun in 1..99
    {
        for verb in 1..99
        {
            let mut prog = proto_prog.clone();
            prog[1] = noun;
            prog[2] = verb;
            prog.next();
            if prog[0] == 19690720
            {
                println!("Day 2 Part 2: {}", noun * 100 + verb);
                return;
            }
        }
    }
}