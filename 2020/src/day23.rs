use std::collections::LinkedList;

type InputType = LinkedList<usize>;
#[aoc_generator(day23)]
pub fn parse_input(buf :&str) -> InputType
{
    buf.bytes().map(|b| (b - b'0') as usize).collect()
}

#[aoc(day23, part1)]
pub fn part1(input : &InputType) -> String
{
    let mut cups = input.clone();
    let max_cup = *cups.iter().max().unwrap();
    let mut cursor = cups.cursor_front_mut();

    for _ in 0..100
    {
        let current_cup = if let Some(x) = cursor.current() {*x} else { cursor.move_next(); *cursor.current().unwrap() };
        cursor.move_next();

        let mut removed_list = LinkedList::new();
        for _ in 0..3
        {
            if cursor.current().is_none() { cursor.move_next(); }
            removed_list.append(&mut cursor.remove_current_as_list().unwrap());
        }

        let mut target = current_cup-1;
        if target == 0 { target = max_cup; }
        while removed_list.contains(&target)
        {
            target -= 1;
            if target == 0 { target = max_cup; }
        }

        while cursor.as_cursor().current() != Some(&target)
        {
            cursor.move_next();
        }
        cursor.splice_after(removed_list);

        while cursor.as_cursor().current() != Some(&current_cup)
        {
            cursor.move_next();
        }
        cursor.move_next();
    }
    
    while cursor.as_cursor().current() != Some(&1)
    {
        cursor.move_next();
    }

    let mut result = String::new();
    for _ in 0..input.len()-1
    {
        cursor.move_next();
        if cursor.current().is_none()
        {
            cursor.move_next();
        }

        result.push_str(&cursor.current().unwrap().to_string());
    }
    result
}

#[aoc(day23, part1, part1_arr)]
pub fn part1_arr(input : &InputType) -> String
{
    let mut cups = vec![0; input.len()+1];
    let mut cursor = input.cursor_front();
    let mut current_cup = *cursor.current().unwrap();
    let max_cup = *input.iter().max().unwrap();

    while let Some(x) = cursor.peek_next()
    {
        cups[*cursor.current().unwrap()] = *x;
        cursor.move_next();
    }
    cups[*cursor.current().unwrap()] = current_cup;

    for _ in 0..100
    {
        let mut taken_cups = [0;3];
        taken_cups[0] = cups[current_cup];
        taken_cups[1] = cups[taken_cups[0]];
        taken_cups[2] = cups[taken_cups[1]];

        let mut target_cup = current_cup - 1;
        if target_cup == 0 { target_cup = max_cup; }
        while taken_cups.contains(&target_cup)
        {
            target_cup -= 1;
            if target_cup == 0 { target_cup = max_cup; }
        }

        cups[current_cup] = cups[taken_cups[2]];
        cups[taken_cups[2]] = cups[target_cup];
        cups[target_cup] = taken_cups[0];
        current_cup = cups[current_cup];
    }

    let mut result = String::new();
    current_cup = 1;
    for _ in 0..8
    {
        result.push_str(&cups[current_cup].to_string());
        current_cup = cups[current_cup];
    }
    result
}

#[aoc(day23, part2)]
pub fn part2(input : &InputType) -> usize
{
    let mut cups = vec![0; 1000000+1];
    let mut cursor = input.cursor_front();
    let mut current_cup = *cursor.current().unwrap();
    let max_cup = 1000000;

    while let Some(x) = cursor.peek_next()
    {
        cups[*cursor.current().unwrap()] = *x;
        cursor.move_next();
    }
    for i in input.len()+1..=1000000
    {
        cups[i] = i+1;
    }

    cups[*cursor.current().unwrap()] = input.len()+1;
    cups[1000000] = current_cup;

    for _ in 0..10000000
    {
        let mut taken_cups = [0;3];
        taken_cups[0] = cups[current_cup];
        taken_cups[1] = cups[taken_cups[0]];
        taken_cups[2] = cups[taken_cups[1]];

        let mut target_cup = current_cup - 1;
        if target_cup == 0 { target_cup = max_cup; }
        while taken_cups.contains(&target_cup)
        {
            target_cup -= 1;
            if target_cup == 0 { target_cup = max_cup; }
        }

        cups[current_cup] = cups[taken_cups[2]];
        cups[taken_cups[2]] = cups[target_cup];
        cups[target_cup] = taken_cups[0];
        current_cup = cups[current_cup];
    }
    
    cups[1] * cups[cups[1]]
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"389125467";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1_arr(&parse_input(TEST_DATA)), "67384529".to_string())
    }

    #[test]
    pub fn part2_test() 
    {
        assert_eq!(part2(&parse_input(TEST_DATA)), 149245887792)
    }
}