use std::collections::HashMap;

pub fn part1(file_data: &str)
{
    let map = orbiters_map(file_data);

    println!("Day 6 Part 1: {}", orbit_checksum(&map, "COM", 0));

}

pub fn part2(file_data: &str)
{
    let map = orbit_map(&file_data);

    println!("Day 6 Part 2: {}", min_transfers(&map));
}

fn orbiters_map(data: &str) -> HashMap<&str, Vec<&str>>
{
    let mut map : HashMap<&str, Vec<&str>> = HashMap::new();
    for line in data.lines()
    {
        let inner = &line[..3];
        let outer = &line[4..];
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

fn orbit_checksum(map: &HashMap<&str, Vec<&str>>, root: &str, depth: u32) -> u32
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

fn orbit_map(data: &str) -> HashMap<&str, &str>
{
    let mut map : HashMap<&str, &str> = HashMap::new();
    for line in data.lines()
    {
        map.insert(&line[4..], &line[..3]);
    }
    map
}

fn min_transfers(map: &HashMap<&str, &str>) -> usize
{
    fn get_path<'a>(map: &'a HashMap<&str, &str>, leaf: &str) -> Vec<&'a str>
    {
        let mut path = Vec::new();
        let mut pos = map.get(leaf).unwrap();
        while *pos != "COM"
        {
            path.push(*pos);
            pos = map.get(pos).unwrap();
        }
        path
    }
    let you_path = get_path(map, "YOU");
    let san_path = get_path(map, "SAN");

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