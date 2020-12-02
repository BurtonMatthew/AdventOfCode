use std::collections::HashMap;
use vec2::Vec2;
use pathfinding::prelude::{astar};

pub fn part1(file_data: &str)
{
    let width = file_data.find(|c| c == '\n').unwrap();
    let mut map = Vec2::from_vec(file_data.chars().filter(|c| *c != '\n').collect(), width);

    let mut start_pos = (0,0);
    let mut end_pos = (0,0);

    let mut tag_pos_map : HashMap<String, (usize, usize)> = HashMap::new();
    let mut warp_map = HashMap::new();
    let mut handle_tag = |tag : String, x, y|
    {
        if tag == "AA" { start_pos = (x,y); }
        else if tag == "ZZ" { end_pos = (x,y); }
        else
        {
            if let Some(pos) = tag_pos_map.get(&tag)
            {
                warp_map.insert(*pos, (x,y));
                warp_map.insert((x,y), *pos);
            }
            else
            {
                tag_pos_map.insert(tag, (x,y));
            }
        }
    };

    let is_letter = |c| c >= 'A' && c <= 'Z';
    for y in 0..map.height()
    {
        for x in 0..map.width()
        {
            if is_letter(map[y][x])
            {
                if is_letter(map[y][x+1])
                {
                    let tag = [map[y][x], map[y][x+1]].iter().cloned().collect::<String>();
                    map[y][x] = ' ';
                    map[y][x+1] = ' ';
                    if x > 0 && map[y][x-1] == '.' { handle_tag(tag, x-1, y); }
                    else { handle_tag(tag, x+2, y); }
                }
                else
                {
                    let tag = [map[y][x], map[y+1][x]].iter().cloned().collect::<String>();
                    map[y][x] = ' ';
                    map[y+1][x] = ' ';
                    if y > 0 && map[y-1][x] == '.' { handle_tag(tag, x, y-1); }
                    else { handle_tag(tag, x, y+2); }
                }
            }
        }
    }

    let successors = |p : &(usize, usize)|
    {
        let mut succ =  Vec::new();

        if p.0 > 0 && map[(p.0-1, p.1)] == '.' { succ.push(((p.0-1, p.1), 1)); }
        if p.0 < map.width()-1 && map[(p.0+1, p.1)] == '.' { succ.push(((p.0+1, p.1), 1)); }
        if p.1 > 0 && map[(p.0, p.1-1)] == '.' { succ.push(((p.0, p.1-1), 1)); }
        if p.1 < map.height()-1 && map[(p.0, p.1+1)] == '.' { succ.push(((p.0, p.1+1), 1)); }

        if let Some(warp) = warp_map.get(&p) { succ.push((*warp, 1)); }

        succ
    };

    let result = astar(&start_pos, successors, |_| 0, |p| *p == end_pos);

    println!("Day 20 part 1: {}", result.unwrap().1);
}

pub fn part2(file_data: &str)
{
    let width = file_data.find(|c| c == '\n').unwrap();
    let mut map = Vec2::from_vec(file_data.chars().filter(|c| *c != '\n').collect(), width);

    let mut start_pos = (0,0,0);
    let mut end_pos = (0,0,0);

    let mut tag_pos_map : HashMap<String, (usize, usize)> = HashMap::new();
    let mut warp_map = HashMap::new();
    let mut handle_tag = |tag : String, x, y|
    {
        if tag == "AA" { start_pos = (x,y,0); }
        else if tag == "ZZ" { end_pos = (x,y,0); }
        else
        {
            if let Some(pos) = tag_pos_map.get(&tag)
            {
                warp_map.insert(*pos, (x,y));
                warp_map.insert((x,y), *pos);
            }
            else
            {
                tag_pos_map.insert(tag, (x,y));
            }
        }
    };

    let is_letter = |c| c >= 'A' && c <= 'Z';
    for y in 0..map.height()
    {
        for x in 0..map.width()
        {
            if is_letter(map[y][x])
            {
                if is_letter(map[y][x+1])
                {
                    let tag = [map[y][x], map[y][x+1]].iter().cloned().collect::<String>();
                    map[y][x] = ' ';
                    map[y][x+1] = ' ';
                    if x > 0 && map[y][x-1] == '.' { handle_tag(tag, x-1, y); }
                    else { handle_tag(tag, x+2, y); }
                }
                else
                {
                    let tag = [map[y][x], map[y+1][x]].iter().cloned().collect::<String>();
                    map[y][x] = ' ';
                    map[y+1][x] = ' ';
                    if y > 0 && map[y-1][x] == '.' { handle_tag(tag, x, y-1); }
                    else { handle_tag(tag, x, y+2); }
                }
            }
        }
    }

    let successors = |p : &(usize, usize, usize)|
    {
        let mut succ =  Vec::new();

        if p.0 > 0 && map[(p.0-1, p.1)] == '.' { succ.push(((p.0-1, p.1, p.2), 1)); }
        if p.0 < map.width()-1 && map[(p.0+1, p.1)] == '.' { succ.push(((p.0+1, p.1, p.2), 1)); }
        if p.1 > 0 && map[(p.0, p.1-1)] == '.' { succ.push(((p.0, p.1-1, p.2), 1)); }
        if p.1 < map.height()-1 && map[(p.0, p.1+1)] == '.' { succ.push(((p.0, p.1+1, p.2), 1)); }

        let inside = p.0 > 5 && p.0 < map.width() -5 && p.1 > 5 && p.1 < map.height() -5;
        if inside || p.2 > 0
        {
            if let Some(warp) = warp_map.get(&(p.0, p.1)) { succ.push(((warp.0, warp.1, if inside { p.2 + 1 } else { p.2 -1 } ), 1)); }
        }

        succ
    };

    let result = astar(&start_pos, successors, |p| p.2 * 30, |p| *p == end_pos);

    println!("Day 20 part 2: {}", result.unwrap().1);
}