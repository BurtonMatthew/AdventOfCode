use std::collections::HashMap;
use std::str::FromStr;

pub enum Rule 
{
    Rule(u32),
    Char(char),
    Opt(Box<Rule>, Box<Rule>),
    Seq(Vec<Box<Rule>>)
}

impl FromStr for Rule
{
    type Err = ();
    fn from_str(s: &str) -> Result<Rule, ()> 
    {
        if s.contains(" | ") 
        {
            let mut toks = s.split(" | ");
            Ok(Rule::Opt(Box::new(toks.next().unwrap().parse()?), Box::new(toks.next().unwrap().parse()?)))
        } 
        else if s.contains("\"") 
        {
            Ok(Rule::Char(s.chars().nth(1).unwrap()))
        } 
        else if s.contains(' ') 
        {
            Ok(Rule::Seq(s.split(' ').map(|p| Box::new(p.parse().unwrap())).collect()))
        } 
        else
        {
            Ok(Rule::Rule(s.parse().unwrap()))
        } 
    }
}

fn match_rule<'a>(rule: &Rule, rules: &'a HashMap<u32, Rule>, to_parse: &'a [char]) -> Vec<&'a [char]> 
{
    if !to_parse.is_empty() 
    {
        match rule 
        {
            Rule::Rule(i) => match_rule(rules.get(i).unwrap(), rules, to_parse),
            Rule::Char(c) => 
            {
                if to_parse[0] == *c 
                {
                    vec![&to_parse[1..]]
                } 
                else 
                {
                    vec![]
                }
            }
            Rule::Opt(fst, snd) => 
            {
                let mut next_parses = Vec::new();
                for next_parse in match_rule(fst, rules, to_parse).into_iter() 
                {
                    next_parses.push(next_parse);
                }
                for next_parse in match_rule(snd, rules, to_parse).into_iter() 
                {
                    next_parses.push(next_parse);
                }

                next_parses
            }
            Rule::Seq(s) =>
            {
                let mut next_parses = match_rule(&s[0], rules, to_parse);
                for i in 1..s.len()
                {
                    let mut new_parses = Vec::new();
                    for m in next_parses
                    {
                        new_parses.append(&mut match_rule(&s[i], rules, m));
                    }
                    next_parses = new_parses;
                }
                next_parses
            }
        }
    }
    else
    {
        vec![]
    }
}

pub fn parse_input(input : &str) -> (HashMap<u32, Rule>, Vec<String>)
{
    let mut blocks = input.trim().split("\n\n");
    let rule_block = blocks.next().unwrap();
    let message_block = blocks.next().unwrap();

    let mut rules = HashMap::new();
    for rule in rule_block.trim().lines()
    {
        let mut parts = rule.split(": ");
        let id = parts.next().unwrap().parse().unwrap();
        rules.insert(id, parts.next().unwrap().parse().unwrap());
    }

    let messages = message_block.lines().map(|line| line.to_string()).collect();
    (rules, messages)
}

#[aoc(day19, part1)]
pub fn part1(input : &str) -> usize
{
    let (rules, messages) = parse_input(input);

    let mut num_valid_messages = 0;
    for message in messages
    {
        for to_parse in match_rule(rules.get(&0).unwrap(), &rules, &message.chars().collect::<Vec<_>>()).iter() 
        {
            if to_parse.is_empty() // Whole string consumed
            {
                num_valid_messages += 1;
                break;
            }
        }
    }

    num_valid_messages
}

#[aoc(day19, part2)]
pub fn part2(input : &str) -> usize
{
    let input2 = input.replace("8: 42", "8: 42 | 42 8").replace("11: 42 31", "11: 42 31 | 42 11 31");
    part1(&input2)
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: \"a\"
5: \"b\"

ababbb
bababa
abbbab
aaabbb
aaaabbb";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(&(TEST_DATA)), 2)
    }

    #[test]
    pub fn part2_test() 
    {
        //assert_eq!(part2(&(TEST_DATA)), 0)
    }
}