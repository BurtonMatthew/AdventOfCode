pub fn part1()
{
    println!("Day 4 Part 1: {}", count_remaining_solutions(6,0,0,false,137683,596253));
}

pub fn part2()
{
    println!("Day 4 Part 2: {}", count_remaining_solutions_part2(6,0,1,0,false,137683,596253));
}

fn count_remaining_solutions(num_digits_rem: i32, cur_sum: i32, last_digit:i32, has_double: bool, min:i32, max:i32) -> i32
{
    if num_digits_rem == 1
    {
        if cur_sum*10 < min || cur_sum*10 > max // not 100% correct, but is always safe given our bounds (for correct see part2 solution)
        {
            0
        }
        else if has_double
        {
            10-last_digit
        }
        else
        {
            1
        }
    }
    else
    {
        let mut solutions = 0;
        for i in i32::max(1,last_digit)..10
        {
            solutions += count_remaining_solutions(num_digits_rem-1, (cur_sum * 10) + i, i, has_double || (i == last_digit), min, max);
        }
        solutions
    }
}

fn count_remaining_solutions_part2(num_digits_rem: i32, cur_sum: i32, last_digit:i32, digit_run:i32, has_double: bool, min:i32, max:i32) -> i32
{
    if num_digits_rem == 0
    {
        if cur_sum < min || cur_sum > max
        {
            0
        }
        else if has_double || digit_run == 2
        {
            1
        }
        else
        {
            0
        }
    }
    else
    {
        let mut solutions = 0;
        for i in last_digit..10
        {
            solutions += count_remaining_solutions_part2(num_digits_rem-1, (cur_sum * 10) + i, i, if i == last_digit {digit_run+1} else {1}, has_double || (i != last_digit && digit_run == 2), min, max);
        }
        solutions
    }
}