use pathfinding::prelude::{astar};

pub fn part1(file_data: &str)
{
    let map = file_data.lines().map(|l| l.as_bytes()).collect::<Vec<&[u8]>>();

    let height = map.len();
    let width = map[0].len();
    let mut start_pos = (255,255);
    let mut key_pos = [(255,255); 26];

    for y in 0..height
    {
        for x in 0..width
        {
            let glyph = map[y][x] as char;
            if glyph == '@' { start_pos = (x,y); }
            else if glyph >= 'a' && glyph <= 'z' { key_pos[glyph as usize - 'a' as usize] = (x,y); }
        }
    }

    let start = PathNode { pos: start_pos, keys: [false;26] };
    let successors = |node : &PathNode| 
    {
        let mut succ = Vec::new();
        for pos in &[(1,0), (-1,0), (0,1), (0,-1)]
        {
            let i_pos = (node.pos.0 as i64 + pos.0, node.pos.1 as i64 + pos.1);
            // Don't go out of bounds
            if i_pos.0 < 0 || i_pos.0 > (width as i64) -1 || i_pos.1 < 0 || i_pos.1 > (height as i64)-1 { continue; }
            let u_pos = (i_pos.0 as usize, i_pos.1 as usize);
            let glyph = map[u_pos.1][u_pos.0] as char;

            // Check that its a walkable tile and if it's a door, we have its key
            if glyph != '#' && (glyph < 'A' || glyph > 'Z' || node.keys[glyph as usize - 'A' as usize])
            {
                let mut new_node = PathNode { pos: u_pos, keys: node.keys };
                // If this is a key, pick it up
                if glyph >= 'a' && glyph <= 'z' { new_node.keys[glyph as usize - 'a' as usize] = true; }
                succ.push((new_node,1));
            }
        }
        succ
    };

    let heuristic = | node : &PathNode |
    {
        // number of remaining keys
        node.keys.iter().fold(0, |s, k| if *k { s } else { s+1 })
            // plus dist to nearest unclaimed key
            + key_pos.iter().map(|p| dist(*p,node.pos)).zip(node.keys.iter()).filter(|(_,k)| !*k).map(|(d,_)| d).sum::<i32>()
    };

    let success = |node : &PathNode|
    {
        node.keys.iter().fold(true, |a, b| a && *b)
    };

    let result = astar(&start, successors, heuristic, success);
    println!("Day 18 part 1: {}", result.unwrap().1);
}

pub fn part2(file_data: &str)
{
    println!("Day 18 part 2: {}", 0);
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct PathNode
{
    pos: (usize,usize),
    keys: [bool;26],
}

fn dist(a : (usize, usize), b: (usize, usize)) -> i32
{
    (a.0 as i32 - b.0 as i32).abs() + (a.1 as i32 - b.1 as i32).abs()
}