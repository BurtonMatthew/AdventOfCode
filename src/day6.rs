use std::fs::File;
use std::io::{prelude::*};
use std::collections::HashMap;

pub fn part1()
{
    let mut file = File::open("input/day6.txt").expect("Couldn't find day6 input");
    let mut file_data = String::new();
    file.read_to_string(&mut file_data).expect("Unable to read file");
    let map = orbiters_map(file_data);

    println!("Day 6 Part 1: {}", orbit_checksum(&map, &String::from("COM"), 0));

}

pub fn part2()
{
    let mut file = File::open("input/day6.txt").expect("Couldn't find day6 input");
    let mut file_data = String::new();
    file.read_to_string(&mut file_data).expect("Unable to read file");
    let map = orbit_map(file_data);

    println!("Day 6 Part 2: {}", min_transfers(&map));
}

fn orbiters_map(data: String) -> HashMap<String, Vec<String>>
{
    let mut map : HashMap<String, Vec<String>> = HashMap::new();
    for line in data.lines()
    {
        let inner = line[..3].to_string();
        let outer = line[4..].to_string();
        if let Some(orbiters) = map.get_mut(&inner)
        {
            orbiters.push(outer);
        }
        else
        {
            map.insert(inner, vec![outer]);
        }
    }
    map
}

fn orbit_checksum(map: &HashMap<String, Vec<String>>, root: &String, depth: u32) -> u32
{
    if let Some(orbiters) = map.get(root)
    {
        depth + orbiters.iter().map(|orb| orbit_checksum(&map, &orb, depth+1)).sum::<u32>()
    }
    else
    {
        depth
    }
}

fn orbit_map(data: String) -> HashMap<String, String>
{
    let mut map : HashMap<String, String> = HashMap::new();
    for line in data.lines()
    {
        let inner = line[..3].to_string();
        let outer = line[4..].to_string();
        map.insert(outer, inner);
    }
    map
}

fn min_transfers(map: &HashMap<String, String>) -> usize
{
    fn get_path<'a>(map: &'a HashMap<String, String>, leaf: &String) -> Vec<&'a String>
    {
        let mut path = Vec::new();
        let mut pos = map.get(leaf).unwrap();
        while *pos != String::from("COM")
        {
            path.push(pos);
            pos = map.get(pos).unwrap();
        }
        path
    }
    let you_path = get_path(map, &String::from("YOU"));
    let san_path = get_path(map, &String::from("SAN"));

    let mut transfers = 0;
    for (you_index, orbit) in you_path.iter().enumerate()
    {
        if let Some(san_index) = san_path.iter().position(|o| o == orbit)
        {
            transfers = you_index + san_index;
            break;
        }
    }
    transfers
}