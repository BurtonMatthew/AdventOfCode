use intcode::Program;

pub fn part1(file_data: &str)
{
    let mut prog : Program = file_data.parse().unwrap();
    prog.extend_tape(100);

    // !(A && B && C) && D
    String::from(
                "NOT A J
                OR A J
                AND A J
                AND B J
                AND C J
                NOT J J
                AND D J
                WALK
                ").chars().for_each(|c| prog.push_input(c as i64));

    println!("Day 21 part 1: {}", prog.last().unwrap());
}

pub fn part2(file_data: &str)
{
    let mut prog : Program = file_data.parse().unwrap();
    prog.extend_tape(100);

    // !(A && B && C) && D && (E || H)
    String::from(
        "NOT A J
        OR A J
        AND A J
        AND B J
        AND C J
        NOT J J
        AND D J
        NOT E T
        NOT T T
        OR H T
        AND T J
        RUN
        ").chars().for_each(|c| prog.push_input(c as i64));

    println!("Day 21 part 2: {}", prog.last().unwrap());
}