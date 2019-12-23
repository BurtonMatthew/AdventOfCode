use intcode::Program;

pub fn part1(file_data: &str)
{
    let mut prog : Program = file_data.parse().unwrap();
    prog.extend_tape(1000);

    let mut computers = Vec::new();
    for i in 0..50
    {
        let mut comp = prog.clone();
        comp.push_input(i);
        computers.push(comp);
    }

    loop
    {
        for i in 0..50
        {
            if let Some(dest) = computers[i].next()
            {
                if dest == 255
                {
                    computers[i].next();
                    println!("Day 23 part 1: {}", computers[i].next().unwrap());
                    return;
                }

                let x = computers[i].next().unwrap();
                let y = computers[i].next().unwrap();
                computers[dest as usize].push_input(x);
                computers[dest as usize].push_input(y);
            }
            else
            {
                computers[i].push_input(-1);
            }
        }
    }
}

pub fn part2(file_data: &str)
{
    let mut prog : Program = file_data.parse().unwrap();
    prog.extend_tape(1000);

    let mut computers = Vec::new();
    for i in 0..50
    {
        let mut comp = prog.clone();
        comp.push_input(i);
        computers.push(comp);
    }

    let mut nat_x = 0;
    let mut nat_y = 0;
    let mut prev_nat_y = 1;

    loop
    {
        let mut any_data = false;

        for i in 0..50
        {
            if let Some(dest) = computers[i].next()
            {
                if dest == 255
                {
                    nat_x = computers[i].next().unwrap();
                    nat_y = computers[i].next().unwrap();
                }
                else
                {

                    let x = computers[i].next().unwrap();
                    let y = computers[i].next().unwrap();
                    computers[dest as usize].push_input(x);
                    computers[dest as usize].push_input(y);
                }
                any_data = true;
            }
            else
            {
                computers[i].push_input(-1);
            }
        }
        if !any_data
        {
            if prev_nat_y == nat_y
            {
                println!("Day 23 part 2: {}", nat_y);
                return
            }
            else
            {
                computers[0].push_input(nat_x);
                computers[0].push_input(nat_y);
                prev_nat_y = nat_y;
            }
        }

    }
}