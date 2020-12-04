use std::collections::HashMap;
use std::fmt::Debug;
use std::str::FromStr;
use nom::{ bytes::complete::{tag, take, take_while1, take_while_m_n }, combinator::{all_consuming, map_res}, multi::many1, sequence::preceded};

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

#[aoc(day4, part2, parser_combinator)]
pub fn part2_combinator(input : &Vec<String>) -> usize
{
    input.iter().filter_map(|entry| parse_passport(entry).ok()).count()
}

#[aoc(day4, part2, dict)]
pub fn part2_dict(input : &Vec<String>) -> usize
{
    let mut passports = Vec::new();
    for entry in input
    {
        let mut passport = HashMap::new();
        for token in entry.split(char::is_whitespace)
        {
            let mut toks = token.split(":");
            passport.insert(toks.next().unwrap().to_string(), toks.next().unwrap().to_string());
        }
        passports.push(passport);
    }

    let mut valid = 0;
    for pass in passports
    {
        if !pass.contains_key("byr") { continue; }
        if !pass.contains_key("iyr") { continue; }
        if !pass.contains_key("eyr") { continue; }
        if !pass.contains_key("hgt") { continue; }
        if !pass.contains_key("hcl") { continue; }
        if !pass.contains_key("ecl") { continue; }
        if !pass.contains_key("pid") { continue; }

        let birth_year = pass.get("byr").unwrap().parse::<i32>().unwrap();
        if birth_year < 1920 || birth_year > 2002 { continue; }
        let issue_year = pass.get("iyr").unwrap().parse::<i32>().unwrap();
        if issue_year < 2010 || issue_year > 2020 { continue; }
        let expiration_year = pass.get("eyr").unwrap().parse::<i32>().unwrap();
        if expiration_year < 2020 || expiration_year > 2030 { continue; }

        let hair_color = pass.get("hcl").unwrap();
        if hair_color.chars().nth(0).unwrap() != '#' { continue; }
        if hair_color.chars().count() != 7 { continue; }
        if hair_color.chars().filter(|c| c.is_digit(16)).count() != 6 { continue; }

        if !["amb".to_string(), "blu".to_string(), "brn".to_string(), "gry".to_string(), "grn".to_string(), "hzl".to_string(), "oth".to_string()].contains(pass.get("ecl").unwrap()) { continue; }

        let id = pass.get("pid").unwrap();
        if id.chars().count() != 9 { continue; }
        if id.chars().filter(|c| c.is_numeric()).count() != 9 { continue; }
        
        let height = pass.get("hgt").unwrap();
        if height.ends_with("cm")
        {
            let h = &height[..height.len()-2].parse::<i32>().unwrap();
            if *h < 150 || *h > 193 { continue; }
        }
        else if height.ends_with("in")
        {
            let h = &height[..height.len()-2].parse::<i32>().unwrap();
            if *h < 59 || *h > 76 { continue; }
        }
        else { continue; }

        valid += 1;
    }
    valid
}

#[derive(Debug)]
enum Height
{
    Cm(u32),
    In(u32)
}


#[derive(Debug)]
struct Color
{
    r: u8,
    b: u8,
    g: u8,
}

#[derive(Debug)]
enum EyeColor
{
    Amber,
    Blue,
    Brown,
    Gray,
    Green,
    Hazel,
    Other
}

impl FromStr for EyeColor
{
    type Err = ();
    fn from_str(s: &str) -> Result<EyeColor, ()> 
    {
        match s
        {
            "amb" => Ok(EyeColor::Amber),
            "blu" => Ok(EyeColor::Blue),
            "brn" => Ok(EyeColor::Brown),
            "gry" => Ok(EyeColor::Gray),
            "grn" => Ok(EyeColor::Green),
            "hzl" => Ok(EyeColor::Hazel),
            "oth" => Ok(EyeColor::Other),
            _ => Err(())
        }
    }
}

#[derive(Debug)]
struct Passport
{
    birth_year: i32,
    issue_year: i32,
    expiration_year: i32,
    height: Height,
    hair_color: Color,
    eye_color: EyeColor,
    id: String,
    country_id: Option<i32>
}

impl Passport
{
    pub fn from_elements(elems: Vec<PassportElement>) -> Result<Passport, ()>
    {
        let mut birth_year: Option<i32> = None;
        let mut issue_year: Option<i32> = None;
        let mut expiration_year: Option<i32> = None;
        let mut height: Option<Height> = None;
        let mut hair_color: Option<Color> = None;
        let mut eye_color: Option<EyeColor> = None;
        let mut id: Option<String> = None;
        let mut country_id: Option<i32> = None;

        for elem in elems
        {
            match elem
            {
                PassportElement::BirthYear(x) =>       { if birth_year.is_none() { birth_year = Some(x) } else { return Err(())} }
                PassportElement::IssueYear(x) =>       { if issue_year.is_none() { issue_year = Some(x) } else { return Err(())} }
                PassportElement::ExpirationYear(x) =>  { if expiration_year.is_none() { expiration_year = Some(x) } else { return Err(())} }
                PassportElement::Height(x) =>          { if height.is_none() { height = Some(x) } else { return Err(())} }
                PassportElement::HairColor(x) =>       { if hair_color.is_none() { hair_color = Some(x) } else { return Err(())} }
                PassportElement::EyeColor(x) =>        { if eye_color.is_none() { eye_color = Some(x) } else { return Err(())} }
                PassportElement::Id(x) =>              { if id.is_none() { id = Some(x) } else { return Err(())} }
                PassportElement::CountryId(x) =>       { if country_id.is_none() { country_id = Some(x) } else { return Err(())} }
            }
        }
        
        if birth_year.is_some() && issue_year.is_some() && expiration_year.is_some() && height.is_some()
                                && hair_color.is_some() && eye_color.is_some() && id.is_some()
        {
            Ok(Passport{birth_year: birth_year.unwrap(), issue_year: issue_year.unwrap(), expiration_year: expiration_year.unwrap()
                ,height: height.unwrap(), hair_color: hair_color.unwrap(), eye_color: eye_color.unwrap(), id: id.unwrap(), country_id })
        }
        else
        {
            Err(())
        }
    }
}

// Intermediate union to hold parsed vars
#[derive(Debug)]
enum PassportElement
{
    BirthYear(i32),
    IssueYear(i32),
    ExpirationYear(i32),
    Height(Height),
    HairColor(Color),
    EyeColor(EyeColor),
    Id(String),
    CountryId(i32)
}

fn parse_int(i: &str) -> nom::IResult<&str, i32>
{
    map_res(nom::character::complete::digit1, str::parse)(i)
}

fn parse_int_between(min: i32, max: i32) -> impl Fn(&str) -> nom::IResult<&str, i32>
{
    move |i| nom::combinator::verify(parse_int, |x| *x >= min && *x <= max)(i)
}

fn is_hex_digit(c: char) -> bool 
{
    c.is_digit(16)
}

fn parse_hex_color_channel(i: &str) -> nom::IResult<&str, u8>
{
    map_res(
        take_while_m_n(2, 2, is_hex_digit),
        |c| u8::from_str_radix(c, 16)
      )(i)
}

fn parse_hex_color(i: &str) -> nom::IResult<&str, Color>
{
    let (i,(_, r, g, b)) = nom::sequence::tuple((
        nom::character::complete::char('#'),
        parse_hex_color_channel,
        parse_hex_color_channel,
        parse_hex_color_channel
    ))(i)?;

    Ok((i, Color{r,g,b}))
}

fn parse_birth_year(i: &str) -> nom::IResult<&str, PassportElement>
{
    let (i,yr) =  preceded(tag("byr:"), parse_int_between(1920, 2002))(i)?;
    Ok((i, PassportElement::BirthYear(yr)))
}

fn parse_issue_year(i: &str) -> nom::IResult<&str, PassportElement>
{
    let (i,yr) = preceded(tag("iyr:"), parse_int_between(2010, 2020))(i)?;
    Ok((i, PassportElement::IssueYear(yr)))
}

fn parse_expiration_year(i: &str) -> nom::IResult<&str, PassportElement>
{
    let (i,yr) = preceded(tag("eyr:"), parse_int_between(2020, 2030))(i)?;
    Ok((i, PassportElement::ExpirationYear(yr)))
}

fn parse_height(i: &str) -> nom::IResult<&str, PassportElement>
{
    let (i,(h, tag)) = preceded(tag("hgt:"),
        nom::branch::alt(
        (
            nom::sequence::tuple((parse_int_between(150, 193), tag("cm"))),
            nom::sequence::tuple((parse_int_between(59, 76), tag("in"))),
        )))(i)?;
    
    match tag
    {
        "in" => Ok((i,PassportElement::Height(Height::In(h as u32)))),
        "cm" => Ok((i,PassportElement::Height(Height::Cm(h as u32)))),
        _ => unreachable!()
    }
}

fn parse_hair_color(i: &str) -> nom::IResult<&str, PassportElement>
{
    let (i,clr) = preceded(tag("hcl:"), parse_hex_color)(i)?;
    Ok((i, PassportElement::HairColor(clr)))
}

fn parse_eye_color(i: &str) -> nom::IResult<&str, PassportElement>
{
    let (i,clr) = preceded(tag("ecl:"), map_res(take(3usize), str::parse))(i)?;
    Ok((i, PassportElement::EyeColor(clr)))
}

fn parse_passport_id(i: &str) -> nom::IResult<&str, PassportElement>
{
    let (i, id) = preceded(tag("pid:"), take_while_m_n(9,9, char::is_numeric))(i)?;
    Ok((i, PassportElement::Id(id.to_string())))
}

fn parse_country_id(i: &str) -> nom::IResult<&str, PassportElement>
{
    let (i, id) = preceded(tag("cid:"), map_res(take_while1(char::is_numeric), str::parse))(i)?;
    Ok((i, PassportElement::CountryId(id)))
}

fn parse_passport_element(i: &str) -> nom::IResult<&str, PassportElement>
{
    nom::sequence::terminated(
        nom::branch::alt(
            (parse_birth_year, parse_issue_year, parse_expiration_year, parse_height
            , parse_hair_color, parse_eye_color, parse_passport_id, parse_country_id))
    , nom::character::complete::multispace0)(i)
}

fn parse_passport(i: &str) -> nom::IResult<&str, Passport>
{
    map_res(all_consuming(many1(parse_passport_element)), Passport::from_elements)(i)
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
        assert_eq!(part1(&parse_input(TEST_DATA_1)), 2);
    }

    #[test]
    pub fn part2_test() 
    {
        assert_eq!(part2_combinator(&parse_input(TEST_DATA_2)), 4);
        assert_eq!(part2_dict(&parse_input(TEST_DATA_2)), 4);
    }
}