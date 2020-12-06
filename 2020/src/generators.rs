// Set of commonly used generators to copy/paste from

// Simple lines
type Input_Type = Vec<String>;
#[aoc_generator(dayX)]
pub fn parse_input(buf :&str) -> Input_Type
{
    buf.split("\n\n").map(|s| s.to_string()).collect()
}

// Parsed lines
struct InData
{
    field: u32
}

type Input_Type = Vec<InData>;
#[aoc_generator(dayX)]
pub fn parse_input(buf :&str) -> Input_Type
{
    buf.lines().map(|line|
        {
            let field : u32;
            scan!(line.bytes() => "{}", field);
            InData{field}
        }).collect()
}

// Multi-line chunks separated by double newlines
type Input_Type = Vec<String>;
#[aoc_generator(dayX)]
pub fn parse_input(buf :&str) -> Input_Type
{
    buf.split("\n\n").map(|s| s.to_string()).collect()
}

// 2D character grid
type Input_Type = Vec2<char>;
pub fn parse_input(buf : &str) -> Input_Type
{
    buf.parse().unwrap()
}