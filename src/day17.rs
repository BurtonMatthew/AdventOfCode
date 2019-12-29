use intcode::Program;
use vec2::Vec2;
use itertools::Itertools;

pub fn main(file_data: &str)
{
    part2(file_data, &part1(file_data));
}

pub fn part1(file_data: &str) -> Vec2<char>
{
    let mut program : Program = file_data.parse().unwrap();
    program.extend_tape(10000);
    
    let prog_output = program.map(|i| char::from(i as u8)).collect::<Vec<_>>();
    let width = prog_output.iter().position(|c| c.is_whitespace()).unwrap();

    let map = Vec2::from_vec(prog_output.into_iter().filter(|c| !c.is_whitespace()).collect::<Vec<_>>(), width);

    let mut sum_alignment = 0;
    for x in 1..map.width()-2
    {
        for y in 1..map.height()-2
        {
            if map[y][x] == '#' 
                && map[y-1][x] == '#'
                && map[y+1][x] == '#'
                && map[y][x-1] == '#'
                && map[y][x+1] == '#'
            {
                sum_alignment += x * y;
            }
        }
    }


    println!("Day 17 part 1: {}", sum_alignment);
    map
}

pub fn part2(file_data: &str, map: &Vec2<char>)
{
    let mut pos = map.iter().position(|c| ['v','^','<','>'].contains(c))
                            .map(|p| (p % map.width(), p / map.width()))
                            .unwrap();

    let mut facing = match map[pos]
                    {
                        'v' => (0,1),
                        '^' => (0,-1),
                        '<' => (-1,0),
                        '>' => (1,0),
                        _ => unreachable!()
                    };

    let adv_pos = | (px,py), (ax,ay) | ((px as i32 + ax) as usize, (py as i32 + ay) as usize);
    let turn_right = |f| match f
                    {
                        (0,1) => (-1,0),
                        (0,-1) => (1,0),
                        (-1,0) => (0,-1),
                        (1,0) => (0,1),
                        _ => unreachable!()
                    };

    let mut path = Vec::with_capacity(80);
    loop
    {
        // Go forward greedily
        let mut next_pos = adv_pos(pos, facing);
        let mut run_length = 0;
        while map.get(next_pos) == Some(&'#')
        {
            run_length += 1;
            pos = next_pos;
            next_pos = adv_pos(next_pos, facing);
        }

        if run_length > 0
        {
            path.push(PathMove::Forward(run_length));
        }

        if map.get(adv_pos(pos, turn_right(facing))) == Some(&'#')
        {
            facing = turn_right(facing);
            path.push(PathMove::Right);
        }
        else if map.get(adv_pos(pos, turn_right(turn_right(turn_right(facing))))) == Some(&'#')
        {
            facing = turn_right(turn_right(turn_right(facing)));
            path.push(PathMove::Left);
        }
        else
        {
            break;
        }
    }

    // Compress the path
    let compressed_path = compress(&path).unwrap();
    let path_to_string = |p : &[PathMove]| p.iter()
                                            .map(|pi| match pi
                                            {
                                                PathMove::Forward(x) => x.to_string(),
                                                PathMove::Left => String::from("L"),
                                                PathMove::Right => String::from("R"),
                                                PathMove::A => String::from("A"),
                                                PathMove::B => String::from("B"),
                                                PathMove::C => String::from("C"),
                                            })
                                            .join(",");

    let prog_input = [&compressed_path.0, compressed_path.1, compressed_path.2, compressed_path.3].iter()
        .map(|p| path_to_string(p))
        .join("\n") + "\nn\n";

    let mut prog_data : Vec<i64> = file_data.split(",")
                                .map(|num| num.parse::<i64>())
                                .filter(|num| num.is_ok())
                                .map(|num| num.unwrap())
                                .collect();
    prog_data.reserve(2000);
    for _ in 0..2000
    {
        prog_data.push(0);
    }
    prog_data[0] = 2;
    let mut program = Program::from_tape(prog_data);
    prog_input.chars().for_each(|c| program.push_input(c as i64));

    println!("Day 17 part 2: {}", program.last().unwrap());
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum PathMove
{
    Forward(u8),
    Left,
    Right,
    A,
    B,
    C,
}

fn compress(path: &[PathMove]) -> Option<(Vec<PathMove>, &[PathMove], &[PathMove], &[PathMove])>
{
    return compress_inner(vec![&path], [&[]; 3], 0).map(|(sub_a, sub_b, sub_c)|
    {
        let mut compressed_path = Vec::new();
        let mut cur_path = path;
        while cur_path.len() > 0
        {
            if cur_path.starts_with(sub_a)
            {
                cur_path = &cur_path[sub_a.len()..];
                compressed_path.push(PathMove::A);
            }
            else if cur_path.starts_with(sub_b)
            {
                cur_path = &cur_path[sub_b.len()..];
                compressed_path.push(PathMove::B);
            }
            else
            {
                cur_path = &cur_path[sub_c.len()..];
                compressed_path.push(PathMove::C);
            }
        }
        (compressed_path, sub_a, sub_b, sub_c)
    });

    fn compress_inner<'a>(rem: Vec<&'a[PathMove]>, mut subs: [&'a[PathMove]; 3], depth: usize) -> Option<(&'a[PathMove], &'a[PathMove], &'a[PathMove])>
    {
        if rem.iter().map(|path| path.len()).sum::<usize>() == 0
        {
            return Some((subs[0], subs[1], subs[2]));
        }
        else if depth == 3
        {
            return None;
        }

        for len in 1..usize::min(10, rem[0].len())+1
        {
            let candidate = &rem[0][0..len];
            let split = rem.iter().flat_map(|v| split_vec(v, candidate)).collect::<Vec<&'a[PathMove]>>();
            subs[depth] = candidate;
            if let Some(result) = compress_inner(split, subs, depth+1)
            {
                return Some(result);
            }
        }

        None
    }
}

fn split_vec<'a, T: PartialEq>(input: &'a[T], pattern: &[T]) -> Vec<&'a[T]>
{
    let mut result = Vec::new();
    let mut i = 0;
    let mut search_slice = input;
    while i < ((search_slice.len() + 1).saturating_sub(pattern.len()))
    {
        if search_slice[i..].starts_with(pattern)
        {
            let pre_slice = &search_slice[0..i];
            if pre_slice.len() > 0
            {
                result.push(pre_slice);
            }
            search_slice = &search_slice[i+pattern.len()..];
            i = 0;
        }
        else
        {
            i += 1;
        }
    }
    if search_slice.len() > 0
    {
        result.push(search_slice);
    }
    result
}

#[test]
fn test_split_vec()
{
    // split within list
    let result1: Vec<&[i32]> = vec![&[1], &[4,5,4], &[1]];
    assert_eq!(split_vec(&vec![1,2,3,4,5,4,2,3,1], &vec![2,3]), result1);

    // split not in list
    let result2: Vec<&[i32]> = vec![&[1,2,3,4,5,4,2,3,1]];
    assert_eq!(split_vec(&vec![1,2,3,4,5,4,2,3,1], &vec![5,5]), result2);

    // split start of list
    let result3: Vec<&[i32]> = vec![&[4,5,4,2,3,1]];
    assert_eq!(split_vec(&vec![1,2,3,4,5,4,2,3,1], &vec![1,2,3]), result3);

    // split end of list
    let result4: Vec<&[i32]> = vec![&[1,2,3,4,5,4,2]];
    assert_eq!(split_vec(&vec![1,2,3,4,5,4,2,3,1], &vec![3,1]), result4);

    // split whole list
    let result5: Vec<&[i32]> = vec![];
    assert_eq!(split_vec(&vec![1,2,3,4,5,4,2,3,1], &vec![1,2,3,4,5,4,2,3,1]), result5);
}