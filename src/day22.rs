use modular::ModInteger;

pub fn part1(file_data: &str)
{
    const DECK_SIZE : i128 = 10007;
    let start_pos: ModInteger<i128,{DECK_SIZE}> = 2019.into();
    println!("Day 22 part 1: {}", file_data.lines().fold(start_pos, |p, l| parse_line(p, l)));
}

pub fn part2(file_data: &str)
{
    const DECK_SIZE : i128 = 119315717514047;
    let step = |i| { file_data.lines().rev().fold(i, |p, l| undo_parse_line(p, l)) };
    let pos_0 : ModInteger<i128,{DECK_SIZE}> = 2020.into();
    let pos_1 = step(pos_0);
    let pos_2 = step(pos_1);

    let m = (pos_1 - pos_2) / (pos_0 - pos_1);
    let b = pos_1 - (m * pos_0);

    let iterations: i128 = 101741582076661;
    let result = (m.pow(iterations) * pos_0) + (b * ((m.pow(iterations) - 1) / (m - 1)));
    println!("Day 22 part 2: {}", result);
}

fn parse_line<const MOD:i128>(pos: ModInteger<i128,{MOD}>, line: &str) -> ModInteger<i128,{MOD}>
{
    if line == "deal into new stack"
    {
        deal_stack(pos)
    }
    else if line.starts_with("deal with increment ")
    {
        deal_increment(pos, line[20..].parse::<i128>().unwrap())
    }
    else if line.starts_with("cut")
    {
        cut(pos, line[4..].parse::<i128>().unwrap())
    }
    else
    {
        panic!("Invalid line");
    }
}

fn undo_parse_line<const MOD:i128>(pos: ModInteger<i128,{MOD}>, line: &str) -> ModInteger<i128,{MOD}>
{
    if line == "deal into new stack"
    {
        undo_deal_stack(pos)
    }
    else if line.starts_with("deal with increment ")
    {
        undo_deal_increment(pos, line[20..].parse::<i128>().unwrap())
    }
    else if line.starts_with("cut")
    {
        undo_cut(pos, line[4..].parse::<i128>().unwrap())
    }
    else
    {
        panic!("Invalid line");
    }
}

fn deal_stack<const MOD:i128>(pos: ModInteger<i128,{MOD}>) -> ModInteger<i128,{MOD}>
{
    -pos
}

fn undo_deal_stack<const MOD:i128>(pos: ModInteger<i128,{MOD}>) -> ModInteger<i128,{MOD}>
{
    -pos
}

fn deal_increment<const MOD:i128>(pos: ModInteger<i128,{MOD}>, inc: i128) -> ModInteger<i128,{MOD}>
{
    pos * inc
}

fn undo_deal_increment<const MOD:i128>(pos: ModInteger<i128,{MOD}>, inc: i128) -> ModInteger<i128,{MOD}>
{
    pos / inc
}

fn cut<const MOD:i128>(pos: ModInteger<i128,{MOD}>, cut: i128) -> ModInteger<i128,{MOD}>
{
    pos - cut
}

fn undo_cut<const MOD:i128>(pos: ModInteger<i128,{MOD}>, cut: i128) -> ModInteger<i128,{MOD}>
{
    pos + cut
}

#[test]
fn test_deal()
{
    let deck: Vec<i128> = (0..10).collect();
    //assert_eq!(deal_stack(deck.clone()), vec![9,8,7,6,5,4,3,2,1,0]);
}

#[test]
fn test_undo_deal()
{
    for (s,r) in [(9,0), (4,5), (0,9), (7,2)].iter()
    {
        let modint: ModInteger<i128,10> = ModInteger::from(*s as i128);
        let res : i128 = undo_deal_stack(modint).into();
        assert_eq!(res, *r as i128);
    }
}

#[test]
fn test_cut()
{
    let deck: Vec<i128> = (0..10).collect();
    //assert_eq!(cut(deck.clone(), 3), vec![3,4,5,6,7,8,9,0,1,2]);
    //assert_eq!(cut(deck.clone(), -4), vec![6,7,8,9,0,1,2,3,4,5]);
}

#[test]
fn test_undo_cut()
{
    for (s,c,r) in [(3,3,6), (0,3,3), (8,3,1), (3,-4,9),(0,-4,6),(8,-4,4)].iter()
    {
        let modint: ModInteger<i128,10> = ModInteger::from(*s as i128);
        let res : i128 = undo_cut(modint, *c).into();
        assert_eq!(res, *r as i128);
    }
}

#[test]
fn test_increment()
{
    let deck: Vec<i128> = (0..10).collect();
    //assert_eq!(deal_increment(deck.clone(), 3), vec![0,7,4,1,8,5,2,9,6,3]);
}

#[test]
fn test_undo_increment()
{
    for (s,r) in [(0,0), (1,7), (2,4), (3,1), (4,8), (5,5), (6,2), (7,9), (8,6), (9,3)].iter()
    {
        let modint: ModInteger<i128,10> = ModInteger::from(*s as i128);
        let res : i128 = undo_deal_increment(modint, 3).into();
        assert_eq!(res, *r as i128);
    }
}