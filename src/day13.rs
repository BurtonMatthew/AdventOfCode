use intcode::Program;

pub fn part1(file_data: &str)
{
    let mut breakout : Program = file_data.parse().unwrap();
    breakout.extend_tape(1000);

    let mut blocks = 0;
    loop
    {
        if let Some(_) = breakout.next()
        {

        }
        else
        {
            break;
        }
        breakout.next();
        if breakout.next().unwrap() == 2 { blocks += 1}
    }

    println!("Day 13 part 1: {}", blocks);
}

pub fn part2(file_data: &str)
{
    let mut breakout : Program = file_data.parse().unwrap();
    breakout.extend_tape(1000);
    breakout[0] = 2;

    let mut ball_x = 0;
    let mut paddle_x = 0;
    let mut score = 0;
    
    loop
    {
        let x;
        if let Some(x_pos) = breakout.next()
        {
            if x_pos == -1
            {
                breakout.next();
                score = breakout.next().unwrap();
                continue;
            }
            x = x_pos;
        }
        else
        {
            if breakout.needs_input()
            {
                match paddle_x.cmp(&ball_x)
                {
                    std::cmp::Ordering::Less => { breakout.push_input(1); },
                    std::cmp::Ordering::Greater => { breakout.push_input(-1);},
                    std::cmp::Ordering::Equal => { breakout.push_input(0);}
                }
                continue;
            }
            else
            {
                break;
            }
        }

        breakout.next();

        match breakout.next().unwrap()
        {
            3 => { paddle_x = x; }
            4 => { ball_x = x; }
            _ => { }
        }
    }

    println!("Day 13 part 2: {}", score);
}