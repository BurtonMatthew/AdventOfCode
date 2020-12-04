use vec2::Vec2;
use std::collections::HashMap;


#[aoc_generator(day4)]
pub fn parse_input(buf : &str) -> Vec<HashMap<String, String>>
{

    let mut res = Vec::new();
    for entry in buf.split("\n\n")
    {
        let mut passport = HashMap::new();
        for token in entry.split(char::is_whitespace)
        {
            let mut toks = token.split(":");
            passport.insert(toks.next().unwrap().to_string(), toks.next().unwrap().to_string());
        }
        res.push(passport);
    }
    res
}

#[aoc(day4, part1)]
pub fn part1(input : &Vec<HashMap<String, String>>) -> usize
{
    input.iter().filter(|pass| 
        {
            pass.contains_key("byr") &&
            pass.contains_key("iyr") &&
            pass.contains_key("eyr") &&
            pass.contains_key("hgt") &&
            pass.contains_key("hcl") &&
            pass.contains_key("ecl") &&
            pass.contains_key("pid")
        }
        ).count()
}

#[aoc(day4, part2)]
pub fn part2(input : &Vec<HashMap<String, String>>) -> usize
{
    input.iter().filter(|pass| 
    {
        pass.contains_key("byr") && pass.get("byr").unwrap().parse::<i32>().unwrap() >= 1920 && pass.get("byr").unwrap().parse::<i32>().unwrap() <= 2002 &&
        pass.contains_key("iyr") && pass.get("iyr").unwrap().parse::<i32>().unwrap() >= 2010 && pass.get("iyr").unwrap().parse::<i32>().unwrap() <= 2020 &&
        pass.contains_key("eyr") && pass.get("eyr").unwrap().parse::<i32>().unwrap() >= 2020 && pass.get("eyr").unwrap().parse::<i32>().unwrap() <= 2030 &&
        pass.contains_key("hgt") && 
        pass.contains_key("hcl") && pass.get("hcl").unwrap().chars().nth(0).unwrap() == '#' && pass.get("hcl").unwrap().chars().count() == 7 && pass.get("hcl").unwrap().chars().skip(1).filter(|c| char::is_numeric(*c) || "abcdef".contains(*c)).count() == 6 &&
        pass.contains_key("ecl") && ["amb".to_string(), "blu".to_string(), "brn".to_string(), "gry".to_string(), "grn".to_string(), "hzl".to_string(), "oth".to_string()].contains(pass.get("ecl").unwrap()) &&
        pass.contains_key("pid") && pass.get("pid").unwrap().chars().count() == 9 && pass.get("pid").unwrap().chars().filter(|c| char::is_numeric(*c)).count() == 9 &&
        (
            (pass.get("hgt").unwrap().contains("cm") && pass.get("hgt").unwrap().replace("cm", "").parse::<i32>().unwrap() >= 150 && pass.get("hgt").unwrap().replace("cm", "").parse::<i32>().unwrap() <= 193) ||
            (pass.get("hgt").unwrap().contains("in") && pass.get("hgt").unwrap().replace("in", "").parse::<i32>().unwrap() >= 59 && pass.get("hgt").unwrap().replace("in", "").parse::<i32>().unwrap() <= 76) )
    }
    ).count()
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(&parse_input(TEST_DATA)), 2)
    }
}