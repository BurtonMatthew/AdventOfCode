use std::collections::VecDeque;
use std::collections::HashMap;
use pathfinding::prelude::{astar};
use vec2::Vec2;

pub fn part1(file_data: &str)
{
    let width = file_data.find(|c| c == '\n').unwrap() - 1;
    let map = Vec2::from_vec(file_data.chars().filter(|c| !c.is_whitespace()).collect(), width);
    let height = map.height();

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

    let mut position_dists = HashMap::new();

    let mut search_pos = | p |
    {
        let search = bfs(p, &map);
        let key_to_key_dists = key_pos.iter()
                                .zip(0u8..26)
                                .map(|(p,c)| (c, search[*p].clone()) )
                                .filter(|(_, k)| k.0 < i32::max_value() && k.0 > 0)
                                .map(|(key,(dist,doors))| (key,dist,doors))
                                .collect::<Vec<_>>();

        position_dists.insert(p, key_to_key_dists);
    };

    search_pos(start_pos);
    for i in 0..26
    {
        search_pos(key_pos[i]);
    }

    let start = PathNode { pos: start_pos, keys: [false;26] };

    let successors = | node : &PathNode |
    {
        if let Some(destinations) = position_dists.get(&node.pos)
        {
            let mut nodes = Vec::new();
            for destination in destinations
            {
                let key_index = destination.0 as usize;
                if !node.keys[key_index]
                   && destination.2.iter().fold(true, |s,d| s && node.keys[*d as usize - 'A' as usize])
                {
                    let mut new_keys = node.keys.clone();
                    new_keys[key_index] = true;
                    nodes.push( (PathNode { pos: key_pos[key_index], keys: new_keys }, destination.1) );
                }
            }
            nodes
        }
        else
        {
            vec![]
        }
    };

    let heuristic = | node : &PathNode |
    {
        // number of remaining keys
        node.keys.iter().fold(0, |s, k| if *k { s } else { s+1 })
    };

    let success = | node : &PathNode |
    {
        node.keys.iter().fold(true, |a, b| a && *b)
    };

    let result = astar(&start, successors, heuristic, success);
    println!("Day 18 part 1: {}", result.unwrap().1);
}

pub fn part2(file_data: &str)
{
    let width = file_data.find(|c| c == '\n').unwrap() - 1;
    let mut map = Vec2::from_vec(file_data.chars().filter(|c| !c.is_whitespace()).collect(), width);
    let height = map.height();

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

    map[start_pos.0][start_pos.1] = '#';
    map[start_pos.0+1][start_pos.1] = '#';
    map[start_pos.0-1][start_pos.1] = '#';
    map[start_pos.0][start_pos.1+1] = '#';
    map[start_pos.0][start_pos.1-1] = '#';

    let mut position_dists = HashMap::new();

    let mut search_pos = | p |
    {
        let search = bfs(p, &map);
        let key_to_key_dists = key_pos.iter()
                                .zip(0u8..26)
                                .map(|(p,c)| (c, search[*p].clone()) )
                                .filter(|(_, k)| k.0 < i32::max_value() && k.0 > 0)
                                .map(|(key,(dist,doors))| (key,dist,doors))
                                .collect::<Vec<_>>();

        position_dists.insert(p, key_to_key_dists);
    };

    let start_positions = [(start_pos.0-1, start_pos.1-1), (start_pos.0-1, start_pos.1+1), (start_pos.0+1, start_pos.1-1), (start_pos.0+1, start_pos.1+1)];
    start_positions.iter().for_each(|p| search_pos(*p));
    for i in 0..26
    {
        search_pos(key_pos[i]);
    }

    let successors = | node : &PathNode |
    {
        if let Some(destinations) = position_dists.get(&node.pos)
        {
            let mut nodes = Vec::new();
            for destination in destinations
            {
                let key_index = destination.0 as usize;
                if !node.keys[key_index]
                   && destination.2.iter().fold(true, |s,d| s && node.keys[*d as usize - 'A' as usize])
                {
                    let mut new_keys = node.keys.clone();
                    new_keys[key_index] = true;
                    nodes.push( (PathNode { pos: key_pos[key_index], keys: new_keys }, destination.1) );
                }
            }
            nodes
        }
        else
        {
            vec![]
        }
    };

    let heuristic = | node : &PathNode |
    {
        // number of remaining keys
        node.keys.iter().fold(0, |s, k| if *k { s } else { s+1 })
    };

    let success = | node : &PathNode |
    {
        node.keys.iter().fold(true, |a, b| a && *b)
    };

    let mut result = 0;
    for p in &start_positions
    {
        if let Some(destinations) = position_dists.get(&p)
        {
            let mut start = PathNode { pos: *p, keys: [true;26] };
            destinations.iter().map(|d| d.0).for_each(|d| start.keys[d as usize] = false);
            result += astar(&start, successors, heuristic, success).unwrap().1;
        }
    }

    println!("Day 18 part 2: {}", result);
}

fn bfs(start_pos: (usize,usize), map: &Vec2<char>) -> Vec2<(i32, Vec<char>)>
{
    let mut bfs_map = Vec2::from_vec(vec![(i32::max_value(), Vec::new());map.width() * map.height()], map.width());
    let mut search_hull = VecDeque::new();
    search_hull.push_back((start_pos, 0, Vec::new()));

    while let Some(node) = search_hull.pop_front()
    {
        if node.1 >= bfs_map[node.0].0 { continue; }
        let glyph = map[node.0];
        let mut doors = node.2.clone();
        if glyph >= 'A' && glyph <= 'Z' { doors.push(glyph) }
        bfs_map[node.0] = (node.1, doors.clone());

        for pos in &[(1,0), (-1,0), (0,1), (0,-1)]
        {
            let i_pos = ((node.0).0 as i64 + pos.0, (node.0).1 as i64 + pos.1);
            // Don't go out of bounds
            if i_pos.0 < 0 || i_pos.0 >= map.width() as i64 || i_pos.1 < 0 || i_pos.1 >= map.height() as i64 { continue; }
            let u_pos = (i_pos.0 as usize, i_pos.1 as usize);
            let glyph = map[u_pos];

            // Check that its a walkable tile and if it's a door, we have its key
            if glyph != '#'
            {
                // If this is a key, pick it up
                //if glyph >= 'a' && glyph <= 'z' { new_node.keys[glyph as usize - 'a' as usize] = true; }

                search_hull.push_back((u_pos, node.1 +1, doors.clone()));
            }
        }
    }

    bfs_map
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct PathNode
{
    pos: (usize,usize),
    keys: [bool;26],
}