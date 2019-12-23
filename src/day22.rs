use modular::ModInteger;

pub fn part1(file_data: &str)
{
    let mut deck = (0..10007).collect();
    deck = file_data.lines().fold(deck, |d, l| parse_line(d, l));
    println!("Day 22 part 1: {}", deck.iter().position(|&i| i == 2019).unwrap());
}

pub fn part2(file_data: &str)
{
    const DECK_SIZE : i128 = 119315717514047;
    let step = |i| { file_data.lines().rev().fold(i, |p, l| undo_parse_line(p, DECK_SIZE, l)) };
    let pos_0 = 2020;
    let pos_1 = step(pos_0);
    let pos_2 = step(pos_1);

    let m = (pos_1 - pos_2) * mod_inv(pos_0 - pos_1 + DECK_SIZE, DECK_SIZE) % DECK_SIZE;
    let b = pos_1 - (m * pos_0) % DECK_SIZE;

    let _test_int: ModInteger<i128, {DECK_SIZE}> = 8.into();

    let iterations : i128 = 101741582076661;
    //let result = (mod_pow(m, iterations, DECK_SIZE) * pos_0) + (b * ( (mod_pow(m, iterations, DECK_SIZE) - 1) * mod_inv(m-1 + DECK_SIZE, DECK_SIZE) ) );
    let first_term = (mod_pow(m, iterations, DECK_SIZE) * pos_0) % DECK_SIZE;
    let geo_series = ((mod_pow(m, iterations, DECK_SIZE) - 1) * mod_inv(m - 1, DECK_SIZE)) % DECK_SIZE;
    let result = (first_term + b * geo_series) % DECK_SIZE;

    //println!("Day 22 part 2: {}", result % DECK_SIZE);

    let p0: ModInteger<i128, {DECK_SIZE}> = pos_0.into();
    let p1: ModInteger<i128, {DECK_SIZE}> = pos_1.into();
    let p2: ModInteger<i128, {DECK_SIZE}> = pos_2.into();
    let m0: ModInteger<i128, {DECK_SIZE}> = m.into();
    let b0: ModInteger<i128, {DECK_SIZE}> = b.into();

    let ft0 = p0 * m0.pow(iterations);
    let ge0 = (m0.pow(iterations) - 1) / (m0 - 1);
    let re0 = ft0 + b0 * ge0;

    println!("{} {}", pos_0, p0);
    println!("{} {}", pos_1, p1);
    println!("{} {}", pos_2, p2);
    println!("m {} {}", m, m0);
    println!("b {} {}", b, b0);
    println!("FT {} {}", first_term, ft0);
    println!("Geo {} {}", geo_series, ge0);
    println!("Res {} {}", result, re0);

    let result_modint = (m0.pow(iterations) * p0) + (b0 * ((m0.pow(iterations) - 1) / (m0 - 1)));
    println!("Day 22 part 2: {}", result_modint);
}

fn parse_line(deck: Vec<i128>, line: &str) -> Vec<i128>
{
    if line == "deal into new stack"
    {
        deal_stack(deck)
    }
    else if line.starts_with("deal with increment ")
    {
        deal_increment(deck, line[20..].parse::<i128>().unwrap())
    }
    else if line.starts_with("cut")
    {
        cut(deck, line[4..].parse::<i128>().unwrap())
    }
    else
    {
        panic!("Invalid line");
    }
}

fn undo_parse_line(pos: i128, size: i128, line: &str) -> i128
{
    if line == "deal into new stack"
    {
        undo_deal_stack(pos, size)
    }
    else if line.starts_with("deal with increment ")
    {
        undo_deal_increment(pos, line[20..].parse::<i128>().unwrap(), size)
    }
    else if line.starts_with("cut")
    {
        undo_cut(pos, line[4..].parse::<i128>().unwrap(), size)
    }
    else
    {
        panic!("Invalid line");
    }
}

fn deal_stack(mut deck: Vec<i128>) -> Vec<i128>
{
    deck.reverse();
    deck
}

fn undo_deal_stack(pos: i128, size: i128) -> i128
{
    size - pos - 1
}

fn deal_increment(deck: Vec<i128>, inc: i128) -> Vec<i128>
{
    let mut new_deck = deck.clone();
    let mut index = 0;
    deck.iter().for_each(|num|
    {
        new_deck[index] = *num;
        index = (index + inc as usize) % deck.len();
    });
    new_deck
}

fn undo_deal_increment(pos: i128, inc: i128, size: i128) -> i128
{
    mod_inv(inc, size) * pos % size
}

fn mod_inv(a: i128, module: i128) -> i128 
{
    let mut mn = (module, a);
    let mut xy = (0, 1);
   
    while mn.1 != 0 {
      xy = (xy.1, xy.0 - (mn.0 / mn.1) * xy.1);
      mn = (mn.1, mn.0 % mn.1);
    }
   
    while xy.0 < 0 {
      xy.0 += module;
    }
    xy.0
}

fn mod_pow(mut base: i128, mut exp: i128, modulus: i128) -> i128 
{
    if modulus == 1 { return 0 }
    let mut result = 1;
    base = base % modulus;
    while exp > 0 {
        if exp % 2 == 1 {
            result = result * base % modulus;
        }
        exp = exp >> 1;
        base = base * base % modulus
    }
    result
}

fn cut(deck: Vec<i128>, pos: i128) -> Vec<i128>
{
    let len = deck.len();
    deck.into_iter().cycle().skip(if pos < 0 { pos + (len as i128) } else { pos } as usize).take(len).collect()
}

fn undo_cut(pos: i128, cut: i128, size: i128) -> i128
{
    (pos + cut + size) % size
}

#[test]
fn test_deal()
{
    let deck: Vec<i128> = (0..10).collect();
    assert_eq!(deal_stack(deck.clone()), vec![9,8,7,6,5,4,3,2,1,0]);
}

#[test]
fn test_undo_deal()
{
    assert_eq!(undo_deal_stack(9, 10), 0);
    assert_eq!(undo_deal_stack(4, 10), 5);
    assert_eq!(undo_deal_stack(0, 10), 9);
    assert_eq!(undo_deal_stack(7, 10), 2);
}

#[test]
fn test_cut()
{
    let deck: Vec<i128> = (0..10).collect();
    assert_eq!(cut(deck.clone(), 3), vec![3,4,5,6,7,8,9,0,1,2]);
    assert_eq!(cut(deck.clone(), -4), vec![6,7,8,9,0,1,2,3,4,5]);
}

#[test]
fn test_undo_cut()
{
    assert_eq!(undo_cut(3, 3, 10), 6);
    assert_eq!(undo_cut(0, 3, 10), 3);
    assert_eq!(undo_cut(8, 3, 10), 1);
    assert_eq!(undo_cut(3, -4, 10), 9);
    assert_eq!(undo_cut(0, -4, 10), 6);
    assert_eq!(undo_cut(8, -4, 10), 4);
}

#[test]
fn test_increment()
{
    let deck: Vec<i128> = (0..10).collect();
    assert_eq!(deal_increment(deck.clone(), 3), vec![0,7,4,1,8,5,2,9,6,3]);
}

#[test]
fn test_undo_increment()
{
    assert_eq!(undo_deal_increment(0, 3, 10), 0);
    assert_eq!(undo_deal_increment(1, 3, 10), 7);
    assert_eq!(undo_deal_increment(2, 3, 10), 4);
    assert_eq!(undo_deal_increment(3, 3, 10), 1);
    assert_eq!(undo_deal_increment(4, 3, 10), 8);
    assert_eq!(undo_deal_increment(5, 3, 10), 5);
    assert_eq!(undo_deal_increment(6, 3, 10), 2);
    assert_eq!(undo_deal_increment(7, 3, 10), 9);
    assert_eq!(undo_deal_increment(8, 3, 10), 6);
    assert_eq!(undo_deal_increment(9, 3, 10), 3);
}