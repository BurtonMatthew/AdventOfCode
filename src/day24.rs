use vec2::Vec2;

pub fn part1(file_data: &str)
{
    let start_map = Vec2::from_vec(file_data.chars().filter(|c| !c.is_whitespace()).collect::<Vec<_>>(),5);
    let mut prev_maps = vec![start_map];

    loop
    {
        let map = prev_maps.last().unwrap();
        let mut new_map = map.clone();
        for y in 0..map.height()
        {
            for x in 0..map.width()
            {
                let mut adj_bugs = 0;
                if x > 0 && map[(x-1,y)] == '#' { adj_bugs += 1; }
                if x < map.width() -1 && map[(x+1,y)] == '#' { adj_bugs += 1; }
                if y > 0 && map[(x,y-1)] == '#' { adj_bugs += 1; }
                if y < map.height() -1 && map[(x,y+1)] == '#' { adj_bugs += 1; }

                if new_map[(x,y)] == '#'
                {
                    if adj_bugs != 1
                    {
                        new_map[(x,y)] = '.';
                    }
                }
                else
                {
                    if adj_bugs == 1 || adj_bugs == 2
                    {
                        new_map[(x,y)] = '#';
                    }
                }
            }
        }

        if prev_maps.contains(&new_map)
        {
            let mut tile = 0;
            let mut bio_rating = 0;
            for y in 0..map.height()
            {
                for x in 0..map.width()
                {
                    if new_map[(x,y)] == '#'
                    {
                        bio_rating += u64::pow(2, tile);
                    }
                    tile += 1;
                }
            }

            println!("Day 24 part 1: {}", bio_rating);
            return;
        }
        else
        {
            prev_maps.push(new_map);
        }
    }

}

pub fn part2(file_data: &str)
{
    let start_map = Vec2::from_vec(file_data.chars().filter(|c| !c.is_whitespace()).collect::<Vec<_>>(),5);
    
    let mut map = vec![Vec2::from_vec(vec!['.';25],5); 204];
    map[102] = start_map;

    for _ in 0..200
    {
        let mut next_map = map.clone();
        for z in 1..map.len()-1
        {
            for y in 0..5
            {
                for x in 0..5
                {
                    let mut adj_bugs = 0;
                    for pos in adj_recursive(x, y, z)
                    {
                        if map[pos.2][(pos.0, pos.1)] == '#'
                        {
                            adj_bugs += 1;
                        }
                    }

                    if next_map[z][(x,y)] == '#'
                    {
                        if adj_bugs != 1
                        {
                            next_map[z][(x,y)] = '.';
                        }
                    }
                    else
                    {
                        if adj_bugs == 1 || adj_bugs == 2
                        {
                            next_map[z][(x,y)] = '#';
                        }
                    }
                }
            }
        }
        map = next_map;
    }

    let mut bugs = 0;
    for z in 0..map.len()
    {
        for y in 0..5
        {
            for x in 0..5
            {
                if map[z][(x,y)] == '#'
                {
                    bugs += 1;
                }
            }
        }
    }

    println!("Day 24 part 2: {}" , bugs);
}

fn adj_recursive(x: usize, y: usize, z:usize) -> Vec<(usize,usize,usize)>
{
    if x == 2 && y == 2 { return vec![]; }

    let mut results = Vec::new();
    if y == 0
    {
        results.push((2,1,z-1));
        results.push((x,1,z));
    }
    else if y == 4
    {
        results.push((2,3,z-1));
        results.push((x,3,z));
    }
    else if x == 2 && y == 1
    {
        results.push((2,0,z));
        results.push((0,0,z+1));
        results.push((1,0,z+1));
        results.push((2,0,z+1));
        results.push((3,0,z+1));
        results.push((4,0,z+1));
    }
    else if x == 2 && y == 3
    {
        results.push((2,4,z));
        results.push((0,4,z+1));
        results.push((1,4,z+1));
        results.push((2,4,z+1));
        results.push((3,4,z+1));
        results.push((4,4,z+1));
    }
    else
    {
        results.push((x,y-1,z));
        results.push((x,y+1,z));
    }

    if x == 0
    {
        results.push((1,2,z-1));
        results.push((1,y,z));
    }
    else if x == 4
    {
        results.push((3,2,z-1));
        results.push((3,y,z));
    }
    else if y == 2 && x == 1
    {
        results.push((0,2,z));
        results.push((0,0,z+1));
        results.push((0,1,z+1));
        results.push((0,2,z+1));
        results.push((0,3,z+1));
        results.push((0,4,z+1));
    }
    else if y == 2 && x == 3
    {
        results.push((4,2,z));
        results.push((4,0,z+1));
        results.push((4,1,z+1));
        results.push((4,2,z+1));
        results.push((4,3,z+1));
        results.push((4,4,z+1));
    }
    else
    {
        results.push((x-1,y,z));
        results.push((x+1,y,z));
    }

    results
}