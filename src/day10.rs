use std::fs::File;
use std::io::{prelude::*};
use std::collections::HashSet;
use std::collections::HashMap;

pub fn part1() -> (i32,i32)
{
    let mut file = File::open("input/day10.txt").expect("Couldn't find day10 input");
    let mut file_data = String::new();
    file.read_to_string(&mut file_data).expect("Unable to read file");
    
    let mut asteroids = HashSet::new();

    let mut x : i32;
    let mut y : i32 = 0;
    for line in file_data.lines()
    {
        x = 0;
        for c in line.chars()
        {
            if c == '#'
            {
                asteroids.insert((x,y));
            }
            x += 1;
        }
        y += 1;
    }

    let best_visible = &asteroids.iter().map(|a1|
        {
            let mut seen = HashSet::new();
            for a2 in &asteroids
            {
                if a1 == a2 { continue; }
                let dy = a2.1 - a1.1;
                let dx = a2.0 - a1.0;
                let gcd = gcd(dx.abs(),dy.abs());
                seen.insert((dy / gcd, dx / gcd));
            }
            (seen.len(), a1.0, a1.1)
        }).max().unwrap();

    println!("Day 10 part 1: {}", best_visible.0);
    (best_visible.1, best_visible.2)
}

pub fn part2(station: (i32, i32))
{
    let mut file = File::open("input/day10.txt").expect("Couldn't find day10 input");
    let mut file_data = String::new();
    file.read_to_string(&mut file_data).expect("Unable to read file");

    // Parse the asteroid map
    let mut asteroids = HashSet::new();

    let mut x : i32;
    let mut y : i32 = 0;
    for line in file_data.lines()
    {
        x = 0;
        for c in line.chars()
        {
            if c == '#'
            {
                asteroids.insert((x,y));
            }
            x += 1;
        }
        y += 1;
    }

    // Group asteroids by having the same angle from the station
    let mut angle_map : HashMap<(i32,i32), Vec<(i32,i32)>> = HashMap::new();
    for a1 in asteroids
    {
        if a1 == station { continue; }
        let dy = station.1 - a1.1;
        let dx = station.0 - a1.0;
        let gcd = gcd(dx.abs(),dy.abs());
        let key = (dx / gcd, dy / gcd);
        if let Some(vec) = angle_map.get_mut(&key)
        {
            vec.push(a1);
        }
        else
        {
            angle_map.insert(key, vec![a1]);
        }
    }

    // Sort each group sublist of asteroids by its proximity to the station
    for (_, asteroids) in &mut angle_map
    {
        asteroids.sort_by(|(x1,y1), (x2,y2)| ((station.0 - x1).abs() + (station.1 - y1).abs()).cmp(&((station.0 - x2).abs() + (station.1 - y2).abs())));
    }

    // Sort the sublists by their angle from the north vector
    let mut angle_vec : Vec<(&(i32,i32), &mut Vec<(i32,i32)>)> = angle_map.iter_mut().collect();
    angle_vec.sort_by(|(key1, _), (key2, _)|
    {
        angle(key1.0, key1.1).partial_cmp(&angle(key2.0, key2.1)).unwrap()
    });

    // Round robin through the sublists removing elements
    let mut cur_index = 0;
    for _ in 0..199
    {
        angle_vec[cur_index].1.remove(0);
        if angle_vec[cur_index].1.len() == 0
        {
            angle_vec.remove(cur_index);
        }
        else
        {
            cur_index += 1;
        }
        cur_index %= angle_vec.len();
    }

    println!("Day 10 part 2: {}", angle_vec[cur_index].1[0].0 * 100 + angle_vec[cur_index].1[0].1);


}

fn gcd(mut a: i32, mut b: i32) -> i32 
{
    while b != 0 
    {
        let tmp = a;
        a = b;
        b = tmp % b;
    }
    a
}

fn angle(dx: i32, dy: i32) -> f64
{
    let mut result = f64::atan2((-dx).into(), (dy).into());
    if result < 0.0
    {
        result += 2.0 * std::f64::consts::PI;
    }
    result
}