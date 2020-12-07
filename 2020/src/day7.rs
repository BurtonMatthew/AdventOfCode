use std::collections::HashMap;
use std::collections::HashSet;
use std::str::FromStr;
use fn_memo::{FnMemo, unsync::memoize, recur_fn::recur_fn};

type InputType = HashMap<String, Vec<(String,u32)>>;
#[aoc_generator(day7)]
pub fn parse_input(buf :&str) -> InputType
{
    let mut result = HashMap::new();
    for line in buf.lines()
    {
        let line_sanitized = line.replace("bags", "bag").replace(".", "");
        let mut key_val = line_sanitized.split("contain");
        let key = key_val.next().unwrap().trim().to_string();
        let vals = key_val.next().unwrap();
        let mut contain_list = Vec::new();
        if vals != " no other bag"
        {
            for tok in vals.split(",")
            {
                let num: u32 = u32::from_str(&tok.chars().filter(|&c| c.is_numeric()).collect::<String>()).unwrap();
                let bag_type: String = tok.chars().filter(|&c| !c.is_numeric()).collect();
                contain_list.push((bag_type.trim().to_string(), num));
            }
        }

        if contain_list.len() > 0
        {
            result.insert(key, contain_list);
        }
    }
    result
}

#[aoc(day7, part1)]
pub fn part1(input : &InputType) -> usize
{
    let mut containing: HashSet<String> = HashSet::new();
    let mut search: Vec<String> = vec!("shiny gold bag".to_string());

    while search.len() > 0
    {
        let key_bag = search.pop().unwrap();
        for (key, bags) in input
        {
            if bags.iter().filter(|(bag, _)| *bag == key_bag).count() > 0
            {
                if !containing.contains(key)
                {
                    containing.insert(key.clone());
                    search.push(key.clone());
                }
            }
        }
    }

    containing.len()
}

#[aoc(day7, part2)]
pub fn part2(input : &InputType) -> u32
{
    get_contained_bags(&"shiny gold bag".to_string(), &mut HashMap::with_capacity(input.len()), input)
}

pub fn get_contained_bags(root_bag: &String, cache: &mut HashMap<String, u32>, input : &InputType) -> u32
{
    if let Some(contained_bag_types) = input.get(root_bag)
    {
        let mut contained_bags = 0;
        for (bag, num) in contained_bag_types
        {
            if let Some(cached) = cache.get(bag)
            {
                contained_bags += num * (cached+1);
            }
            else
            {
                let calc = get_contained_bags(bag, cache, input);
                cache.insert(bag.clone(), calc);
                contained_bags += num * (calc+1);
            }
        }
        contained_bags
    }
    else
    {
        0
    }
}

#[aoc(day7, part2, part2_recursive_memo)]
pub fn part2_recursive_memo(input : &InputType) -> u32
{
    let get_contained_bags = memoize(recur_fn(|get_contained_bags, root_bag: &String| {
        input.get(root_bag)
            .map(|contained_bags| contained_bags.iter().fold(0, |total, (bag, num)| total + num * (get_contained_bags(bag) + 1)))
            .unwrap_or(0)
    }));
    get_contained_bags.call(&"shiny gold bag".to_string())
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(&parse_input(TEST_DATA)), 4)
    }

    #[test]
    pub fn part2_test() 
    {
        assert_eq!(part2(&parse_input(TEST_DATA)), 32)
    }
}