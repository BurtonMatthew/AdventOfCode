use intcode::Program;

pub fn part1(file_data: &str)
{
    let mut prog_data : Vec<i64> = file_data.split(",")
                                .map(|num| num.parse::<i64>())
                                .filter(|num| num.is_ok())
                                .map(|num| num.unwrap())
                                .collect();
    prog_data.reserve(1000);
    for _ in 0..100
    {
        prog_data.push(0);
    }

    let mut sum = 0;
    for x in 0..50
    {
        for y in 0..50
        {
            let mut program = Program::from_tape(prog_data.clone());
            program.push_input(x);
            program.push_input(y);
            sum += program.next().unwrap();
        }
    }

    println!("Day 19 part 1: {}", sum);
}

pub fn part2(file_data: &str)
{
    let mut prog_data : Vec<i64> = file_data.split(",")
                                .map(|num| num.parse::<i64>())
                                .filter(|num| num.is_ok())
                                .map(|num| num.unwrap())
                                .collect();
    prog_data.reserve(1000);
    for _ in 0..100
    {
        prog_data.push(0);
    }

    let get = |x,y| 
    {
        let mut program = Program::from_tape(prog_data.clone());
        program.push_input(x as i64);
        program.push_input(y as i64);
        program.next().unwrap() == 1
    };

    // look for bottom left corner
    let mut y : usize = 100;
    let mut x : usize = 0;
    loop
    {
        // step back in x to account for widening beam
        x = x.saturating_sub(1);
        
        // advance till we're in the beam
        while !get(x,y)
        {
            x += 1;
        }
        if get(x+99, y) && get(x, y-99) && get (x+99, y-99)
        {
            println!("Day 19 part 2: {}", (x * 10000) + y - 99);
            break;
        }
        y += 1;
    }

}