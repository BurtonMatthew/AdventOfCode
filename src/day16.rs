use std::iter;

pub fn part1(file_data: &str)
{
    let mut signal = file_data.chars().map(|c| c.to_digit(10).unwrap() as i64 ).collect::<Vec<_>>();

    for _ in 0..100
    {
        for i in 0..signal.len()
        {
            signal[i] = calc_digit(i+1, &signal);
        }
    }

    println!("Day 16 part 1: {:08}", signal.iter().take(8).fold(0, |s, i| s*10 + i));
}

pub fn part2(file_data: &str)
{
    let mut init_signal = file_data.chars().map(|c| c.to_digit(10).unwrap() as i64 ).cycle().take(file_data.len() * 10000).collect::<Vec<_>>();
    let skip_num = init_signal.iter().take(7).fold(0, |s, i| s*10 + i) as usize;

    let signal = &mut init_signal[skip_num..];

    for _ in 0..100
    {
        let mut signal_sum = signal.iter().sum::<i64>();
        for i in 0..signal.len()
        {
            let cur_value = signal[i];
            signal[i] = signal_sum % 10;
            signal_sum -= cur_value;
        }
    }

    println!("Day 16 part 2: {:08}", signal.iter().take(8).fold(0, |s, i| s*10 + i));
}

fn calc_digit(digit: usize, signal: &Vec<i64>) -> i64
{
    let cycle = [0,1,0,-1].iter().flat_map(|i| iter::repeat(i).take(digit)).cycle();
    signal.iter().zip(cycle.skip(1)).map(|(x,y)| (x * y)).sum::<i64>().abs() % 10
}