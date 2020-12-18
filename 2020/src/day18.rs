use std::str;

#[derive(Debug, Clone, PartialEq)]
enum Op
{
    Add,
    Mul
}

#[aoc(day18, part1)]
pub fn part1(input : &str) -> usize
{
    input.lines().map(|l| parse_expr(&l.bytes().collect::<Vec<_>>(), &mut 0)).sum()
}

pub fn parse_expr(expr: &[u8], idx: &mut usize) -> usize
{
    let mut ops: Vec<Op> = Vec::new();
    let mut vals: Vec<usize> = Vec::new();

    while *idx != expr.len() && expr[*idx] != b')'
    {
        if expr[*idx] == b'('
        {
            *idx += 1;
            vals.push(parse_expr(expr, idx));
        }
        else if expr[*idx] >= b'0' && expr[*idx] <= b'9'
        {
            vals.push(parse_num(&expr, idx));
        }
        else if expr[*idx] == b'+'
        {
            ops.push(Op::Add);
            *idx += 1;
        }
        else if expr[*idx] == b'*'
        {
            ops.push(Op::Mul);
            *idx += 1;
        }
        else
        {
            *idx += 1;
        }
    }

    if *idx != expr.len()
    {
        *idx += 1;
    }

    let mut total = vals[0];
    for (i,op) in ops.iter().enumerate()
    {
        match op
        {
            Op::Add => { total += vals[i+1]; }
            Op::Mul => { total *= vals[i+1]; }
        }
    }
    total
}

pub fn parse_num(expr: &[u8], idx: &mut usize) -> usize
{
    let start_idx = idx.clone();
    while *idx < expr.len() && expr[*idx] >= b'0' && expr[*idx] <= b'9'
    {
        *idx += 1;
    }

    str::from_utf8(&expr[start_idx..*idx]).unwrap().parse().unwrap()
}

#[aoc(day18, part2)]
pub fn part2(input : &str) -> usize
{
    input.lines().map(|l| parse_expr_2(&l.bytes().collect::<Vec<_>>(), &mut 0)).sum()
}

pub fn parse_expr_2(expr: &[u8], idx: &mut usize) -> usize
{
    let mut ops: Vec<Op> = Vec::new();
    let mut vals: Vec<usize> = Vec::new();

    while *idx != expr.len() && expr[*idx] != b')'
    {
        if expr[*idx] == b'('
        {
            *idx += 1;
            vals.push(parse_expr_2(expr, idx));
        }
        else if expr[*idx] >= b'0' && expr[*idx] <= b'9'
        {
            vals.push(parse_num(&expr, idx));
        }
        else if expr[*idx] == b'+'
        {
            ops.push(Op::Add);
            *idx += 1;
        }
        else if expr[*idx] == b'*'
        {
            ops.push(Op::Mul);
            *idx += 1;
        }
        else
        {
            *idx += 1;
        }
    }

    if *idx != expr.len()
    {
        *idx += 1;
    }

    // Parse Add ops first (highest precedence)
    let mut i = 0;
    let mut num_ops = ops.len();
    while i < num_ops
    {
        if ops[i] == Op::Add
        {
            ops.remove(i);
            num_ops -= 1;
            let sum = vals[i] + vals[i+1];
            vals[i] = sum;
            vals.remove(i+1);
        }
        else
        {
            i += 1;
        }
    }

    vals.iter().product()
}


#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA_1: &str = "2 * 3 + (4 * 5)";
    const TEST_DATA_2: &str = "5 + (8 * 3 + 9 + 3 * 4 * 3)";
    const TEST_DATA_3: &str = "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))";
    const TEST_DATA_4: &str = "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(&(TEST_DATA_1)), 26);
        assert_eq!(part1(&(TEST_DATA_2)), 437);
        assert_eq!(part1(&(TEST_DATA_3)), 12240);
        assert_eq!(part1(&(TEST_DATA_4)), 13632);
    }

    #[test]
    pub fn part2_test() 
    {
        assert_eq!(part2(&(TEST_DATA_1)), 46);
        assert_eq!(part2(&(TEST_DATA_2)), 1445);
        assert_eq!(part2(&(TEST_DATA_3)), 669060);
        assert_eq!(part2(&(TEST_DATA_4)), 23340);
    }
}