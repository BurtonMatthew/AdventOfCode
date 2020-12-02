use intcode::Program;

pub fn part1(file_data: &str)
{
    let mut proto_program : Program = file_data.parse().unwrap();
    proto_program.extend_tape(100);

    let mut sum = 0;
    for x in 0..50
    {
        for y in 0..50
        {
            let mut program = proto_program.clone();
            program.push_input(x);
            program.push_input(y);
            sum += program.next().unwrap();
        }
    }

    println!("Day 19 part 1: {}", sum);
}

pub fn part2(file_data: &str)
{
    let mut proto_program : Program = file_data.parse().unwrap();
    proto_program.extend_tape(100);

    let get = |x,y| 
    {
        let mut program = proto_program.clone();
        program.push_input(x as i64);
        program.push_input(y as i64);
        program.next().unwrap() == 1
    };

    // look for bottom left corner
    let mut y : usize = 100;
    let mut x : usize = 0;
    loop
    {
        // advance till we're in the beam
        while !get(x,y)
        {
            x += 1;
        }

        // check the square corners
        if get(x+99, y) && get(x, y-99) && get (x+99, y-99)
        {
            println!("Day 19 part 2: {}", (x * 10000) + y - 99);
            break;
        }

        // try next line
        y += 1;
    }

}