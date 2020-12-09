// Set of commonly used generators to copy/paste from

// Simple lines
type InputType = Vec<String>;
#[aoc_generator(dayX)]
pub fn parse_input(buf :&str) -> InputType
{
    buf.lines().collect()
}

// Parsed lines
struct InData
{
    field: u32
}

type InputType = Vec<InData>;
#[aoc_generator(dayX)]
pub fn parse_input(buf :&str) -> InputType
{
    buf.lines().map(|line|
        {
            let field : u32;
            scan!(line.bytes() => "{}", field);
            InData{field}
        }).collect()
}

// Multi-line chunks separated by double newlines
type InputType = Vec<String>;
#[aoc_generator(dayX)]
pub fn parse_input(buf :&str) -> InputType
{
    buf.split("\n\n").map(|s| s.to_string()).collect()
}

// 2D character grid
type InputType = Vec2<char>;
pub fn parse_input(buf : &str) -> InputType
{
    buf.parse().unwrap()
}