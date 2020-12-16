use text_io::scan;
use std::ops::RangeInclusive;

#[derive(Debug, Clone, PartialEq)]
pub struct Rule
{
    name: String,
    ranges: (RangeInclusive<usize>, RangeInclusive<usize>)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Input
{
    rules: Vec<Rule>,
    ticket: Vec<usize>,
    tickets: Vec<Vec<usize>>
}

type InputType = Input;
#[aoc_generator(day16)]
pub fn parse_input(buf :&str) -> InputType
{
    let mut blocks = buf.split("\n\n");
    let rule_block = blocks.next().unwrap();
    let ticket_block = blocks.next().unwrap();
    let tickets_block = blocks.next().unwrap();

    let rules = rule_block.lines()
        .map(|line|
            {
                let name: String;
                let min1: usize;
                let max1: usize;
                let min2: usize;
                let max2: usize;
                scan!(line.bytes() => "{}: {}-{} or {}-{}", name, min1, max1, min2, max2);
                Rule{name, ranges: (min1..=max1, min2..=max2)}
            }).collect();

    let ticket = ticket_block.lines().skip(1).next().unwrap().split(',').map(|i| i.parse().unwrap()).collect();
    let tickets = tickets_block.lines().skip(1)
                .map(|line|
                {
                    line.split(',').map(|i| i.parse().unwrap()).collect()
                }).collect();

    Input { rules, ticket, tickets }
}


#[aoc(day16, part1)]
pub fn part1(input : &InputType) -> usize
{
    let mut sum_invalid = 0;
    for ticket in &input.tickets
    {
        for value in ticket
        {
            let mut valid = false;
            for rule in &input.rules
            {
                valid |= rule.ranges.0.contains(value) || rule.ranges.1.contains(value);
            }

            if !valid
            {
                sum_invalid += value;
            }
        }
    }

    sum_invalid
}

#[aoc(day16, part2)]
pub fn part2(input : &InputType) -> usize
{
    let valid_ticks: Vec<Vec<usize>> = input.tickets.iter().cloned().filter(|ticket|
    {
        let mut valid_ticket = true;
        for value in ticket
        {
            let mut valid = false;
            for rule in &input.rules
            {
                valid |= rule.ranges.0.contains(&value) || rule.ranges.1.contains(&value);
            }

            if !valid
            {
                valid_ticket = false;
            }
        }

        valid_ticket
    }).collect();


    let mut possible_indices: Vec<Vec<usize>> = vec![Vec::new(); input.ticket.len()];
    for (i, rule) in input.rules.iter().enumerate()
    {
        for field in 0..input.ticket.len()
        {
            let mut valid_assignment = true;
            for ticket in &valid_ticks
            {
                valid_assignment &= rule.ranges.0.contains(&ticket[field]) || rule.ranges.1.contains(&ticket[field]);
            }

            if valid_assignment
            {
                possible_indices[i].push(field);
            }
        }
    }

    let mut correct_indices: Vec<usize> = vec![0; input.ticket.len()];
    for _ in 0..input.ticket.len()
    {
        for i in 0..possible_indices.len()
        {
            if possible_indices[i].len() == 1
            {
                let index = possible_indices[i][0];
                for j in 0..possible_indices.len()
                {
                    if let Some(pos) = possible_indices[j].iter().position(|x| *x == index) 
                    {
                        possible_indices[j].swap_remove(pos);
                    }
                }
                correct_indices[i] = index;
            }
        }
    }

    input.rules.iter().enumerate().filter(|(_,r)| r.name.starts_with("departure")).map(|(i,_)| input.ticket[correct_indices[i]]).product()
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(&parse_input(TEST_DATA)), 71)
    }

    #[test]
    pub fn part2_test() 
    {
        // No unit test for part 2
    }
}