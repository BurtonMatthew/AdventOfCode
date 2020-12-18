use std::str;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Op
{
    Add,
    Mul
}

#[aoc(day18, part1)]
pub fn part1(input : &str) -> usize
{
    let op_precedences = [(Op::Add, 0), (Op::Mul, 0)];
    input.lines().map(|l| parse_expr(&l.bytes().collect::<Vec<_>>(), &mut 0, &op_precedences)).sum()
}

pub fn parse_expr(expr: &[u8], idx: &mut usize, op_precedences: &[(Op, usize)]) -> usize
{
    let mut ops: Vec<Op> = Vec::new();
    let mut vals: Vec<usize> = Vec::new();

    while *idx != expr.len()
    {
        if expr[*idx] == b'('
        {
            *idx += 1;
            vals.push(parse_expr(expr, idx, op_precedences));
        }
        else if expr[*idx] == b')'
        {
            *idx += 1;
            break;
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

    let max_precendence = op_precedences.iter().map(|(_,p)| *p).max().unwrap_or(0);
    for current_precendence in 0..=max_precendence
    {
        let current_ops = op_precedences.iter()
                                        .filter(|(_,p)| *p == current_precendence)
                                        .map(|(op,_)| *op)
                                        .collect::<Vec<_>>();
        let mut i = 0;
        let mut num_ops = ops.len();
        while i < num_ops
        {
            if current_ops.contains(&ops[i])
            {
                match ops[i]
                {
                    Op::Add => { vals[i] += vals[i+1]; }
                    Op::Mul => { vals[i] *= vals[i+1]; }
                }
                ops.remove(i);
                num_ops -= 1;
                vals.remove(i+1);
            }
            else
            {
                i += 1;
            }
        }
    }
    vals[0]
}

pub fn parse_num(expr: &[u8], idx: &mut usize) -> usize
{
    let mut result = 0;
    while *idx < expr.len() && expr[*idx] >= b'0' && expr[*idx] <= b'9'
    {
        result *= 10;
        result += (expr[*idx] - b'0') as usize;
        *idx += 1;
    }
    result
}

#[aoc(day18, part2)]
pub fn part2(input : &str) -> usize
{
    let op_precedences = [(Op::Add, 0), (Op::Mul, 1)];
    input.lines().map(|l| parse_expr(&l.bytes().collect::<Vec<_>>(), &mut 0, &op_precedences)).sum()
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