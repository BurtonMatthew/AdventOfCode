use std::collections::HashMap;
use std::collections::HashSet;

type InputType = Vec<(isize,isize)>;
#[aoc_generator(day24)]
pub fn parse_input(buf :&str) -> InputType
{
    let mut result = Vec::new();
    for line in buf.lines()
    {
        let bytes = line.bytes().collect::<Vec<_>>();
        let mut i = 0;
        let mut r = 0;
        let mut q = 0;
        while i < bytes.len()
        {
            if bytes[i] == b'w' { r -=1; i +=1; }
            else if bytes[i] == b'e' { r +=1; i +=1; }
            else if bytes[i] == b'n' && bytes[i+1] == b'w' { q-=1; i+=2; }
            else if bytes[i] == b'n' && bytes[i+1] == b'e' { r+=1; q-=1; i+=2; }
            else if bytes[i] == b's' && bytes[i+1] == b'w' { r-=1; q+=1; i+=2; }
            else if bytes[i] == b's' && bytes[i+1] == b'e' { q+=1; i+=2; }
        }
        result.push((r, q));
    }

    result
}

#[aoc(day24, part1)]
pub fn part1(input : &InputType) -> usize
{
    counts(input.iter())
        .into_iter()
        .filter(|(_, count)| count % 2 == 1)
        .count()
}

pub fn counts<T>(input: impl Iterator<Item=T>) -> HashMap<T, usize>
    where T: Copy + Eq + std::hash::Hash
{
        let mut counts = HashMap::new();
    
        for c in input 
        {
            let entry = counts.entry(c).or_insert(0);
            *entry += 1;
        }
    
        counts
}

#[aoc(day24, part2)]
pub fn part2(input : &InputType) -> usize
{
    let mut black_tiles = HashSet::new();
    for tile in input
    {
        if black_tiles.contains(tile)
        {
            black_tiles.remove(tile);
        }
        else
        {
            black_tiles.insert(*tile);
        }
    }

    for _ in 0..100
    {
        let mut new_black_tiles = black_tiles.clone();

        for q in -150..150
        {
            for r in -150..150
            {
                let neighbours = [(1, 0), (1, -1), (0, -1), (-1, 0), (-1, 1), (0, 1)];
                let n_count: usize = neighbours.iter().map(|(dr,dq)| if black_tiles.contains(&(r+dr, q+dq)) {1} else {0}).sum();
                if !black_tiles.contains(&(r,q))
                {
                    if n_count == 2
                    {
                        new_black_tiles.insert((r,q));
                    }
                }
                else
                {
                    if n_count == 0 || n_count > 2
                    {
                        new_black_tiles.remove(&(r,q));
                    }
                }
            }
        }

        black_tiles = new_black_tiles;
    }
    
    black_tiles.len()
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(&parse_input(TEST_DATA)), 10)
    }

    #[test]
    pub fn part2_test() 
    {
        assert_eq!(part2(&parse_input(TEST_DATA)), 2208)
    }
}