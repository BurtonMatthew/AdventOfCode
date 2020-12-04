use std::collections::HashMap;

#[aoc_generator(day4)]
pub fn parse_input(buf :&str) -> Vec<String>
{
    buf.split("\n\n").map(|s| s.to_string()).collect()
}

#[aoc(day4, part1)]
pub fn part1<'a>(input : &Vec<String>) -> usize
{
    input.iter().filter(|entry| ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"].iter().fold(true, |acc, tag| acc & entry.contains(tag))).count()
}

#[aoc(day4, part2)]
pub fn part2(input : &Vec<String>) -> usize
{
    let mut res = Vec::new();
    for entry in input
    {
        let mut passport = HashMap::new();
        for token in entry.split(char::is_whitespace)
        {
            let mut toks = token.split(":");
            passport.insert(toks.next().unwrap().to_string(), toks.next().unwrap().to_string());
        }
        res.push(passport);
    }

    res.iter().filter(|pass| 
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

    const TEST_DATA_1: &str = 
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

    const TEST_DATA_2: &str =
"eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007

pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(&parse_input(TEST_DATA_1)), 2)
    }

    #[test]
    pub fn part2_test() 
    {
        assert_eq!(part2(&parse_input(TEST_DATA_2)), 4)
    }
}