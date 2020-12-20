use std::fmt;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Piece
{
    id: usize,
    data: [u8; 100],
    edges: [u16; 4],
    edge_keys: [u16; 4]
}

impl fmt::Display for Piece 
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result 
    {
        write!(f, "{}\n", self.id)?;
        for y in 0..10
        {
            for x in 0..10
            {
                write!(f, "{}", self.data[y*10+x] as char)?;
            }

            write!(f, "\n")?;
        }
        write!(f, "Keys: {:?}\n", self.edge_keys)?;
        write!(f, "Edges: {:?}\n", self.edges)?;

        fmt::Result::Ok(())
    }
}

type InputType = Vec<Piece>;
#[aoc_generator(day20)]
pub fn parse_input(buf :&str) -> InputType
{
    buf.split("\n\n")
        .map(|block| 
            {
                let mut lines = block.lines();
                let id = lines.next().unwrap()[5..=8].parse().unwrap();
                let mut data = [0; 100];
                for (i,line) in lines.enumerate()
                {
                    for (j,c) in line.bytes().enumerate()
                    {
                        data[i*10+j] = c;
                    }
                }

                let mut edges = [0;4];
                for i in 0..10
                {
                    edges[0] <<= 1;
                    edges[0] |= if data[i] == b'#' {1} else {0};
                }

                for i in 0..10
                {
                    edges[1] <<= 1;
                    edges[1] |= if data[(i+1)*10-1] == b'#' {1} else {0};
                }

                for i in 0..10
                {
                    edges[2] <<= 1;
                    edges[2] |= if data[i+90] == b'#' {1} else {0};
                }

                for i in 0..10
                {
                    edges[3] <<= 1;
                    edges[3] |= if data[i*10] == b'#' {1} else {0};
                }

                let mut edge_keys =  [0;4];
                for i in 0..4
                {
                    edge_keys[i] = u16::min(edges[i], rev_edge(edges[i]));
                }

                Piece { id, data, edges, edge_keys }
            }).collect()
}

pub fn rev_edge(x: u16) -> u16
{
	let mut ret = 0;
    for i in 0..10
    {
        ret <<= 1;
        ret |= if x & (1 << i) != 0 {1} else {0};
    }
    ret
}

#[aoc(day20, part1)]
pub fn part1(input : &InputType) -> usize
{
    let all_edges = input.iter().flat_map(|piece| piece.edge_keys.iter()).collect::<Vec<_>>();

    input.iter()
        .filter(|piece| piece.edge_keys.iter().filter(|e| all_edges.iter().filter(|ae| e == *ae).count() == 1).count() == 2)
        .map(|piece| piece.id).product()
}

#[aoc(day20, part2)]
pub fn part2(input : &InputType) -> usize
{
    let all_edges = input.iter().flat_map(|piece| piece.edge_keys.iter()).collect::<Vec<_>>();
    // TODO: there might not be a topleft oriented piece in the input, take any corner and manually orient
    let top_left = input.iter().position(|piece| all_edges.iter().filter(|ae| piece.edge_keys[0] == ***ae).count() == 1
                                              && all_edges.iter().filter(|ae| piece.edge_keys[3] == ***ae).count() == 1).unwrap();

    let mut pieces = Vec::with_capacity(input.len());
    pieces.push(input[top_left]);

    let dimensions = (input.len() as f64).sqrt() as usize;
    for x in 1..dimensions
    {
        let match_piece = pieces[x-1];
        let mut target_piece = input[input.iter().position(|piece| match_piece.id != piece.id && piece.edge_keys.contains(&match_piece.edge_keys[1])).unwrap()];
        let target_edge = target_piece.edge_keys.iter().position(|e| match_piece.edge_keys[1] == *e).unwrap();
        for _ in 0..3-target_edge
        {
            target_piece = rotate_piece_90(target_piece);
        }

        if match_piece.edges[1] != target_piece.edges[3]
        {
            target_piece = flip_piece_vertical(target_piece);
        }

        pieces.push(target_piece);
    }

    for y in 1..dimensions
    {
        for x in 0..dimensions
        {
            let match_piece = pieces[(y-1)*dimensions+x];
            let mut target_piece = input[input.iter().position(|piece| match_piece.id != piece.id && piece.edge_keys.contains(&match_piece.edge_keys[2])).unwrap()];
            let target_edge = target_piece.edge_keys.iter().position(|e| match_piece.edge_keys[2] == *e).unwrap();
            for _ in 0..(4-target_edge)%4
            {
                target_piece = rotate_piece_90(target_piece);
            }

            if match_piece.edges[2] != target_piece.edges[0]
            {
                target_piece = flip_piece_horizontal(target_piece);
            }

            pieces.push(target_piece);
        }
    }

    let map_dims = 8*dimensions;
    let mut map = vec![0; map_dims*map_dims];
    for py in 0..dimensions
    {
        for px in 0..dimensions
        {
            for y in 1..9
            {
                for x in 1..9
                {
                    map[py*map_dims*8+px*8+(y-1)*map_dims+x-1] = pieces[py*dimensions+px].data[y*10+x];
                }
            }
        }
    }

    let mon = 
"                  # 
#    ##    ##    ###
 #  #  #  #  #  #   ";

    let mon_width = mon.lines().next().unwrap().len();
    let mon_height = mon.lines().count();
    let mut mon_pos = Vec::new();
    for (y, line) in mon.lines().enumerate()
    {
        for (x, c) in line.chars().enumerate()
        {
            if c == '#'
            {
                mon_pos.push((y,x));
            }
        }
    }

    let mut num_monsters = 0;
    for _ in 0..2
    {
        for _ in 0..4
        {
            for y in 0..map_dims-mon_height
            {
                for x in 0..map_dims-mon_width
                {
                    if mon_pos.iter().filter(|(my,mx)| map[(y+my)*map_dims+(x+mx)] == b'#').count() == mon_pos.len()
                    {
                       num_monsters += 1;
                    }
                }
            }
            rotate_90_clockwise(&mut map, map_dims);
        }
        flip_vertical(&mut map, map_dims);
    }

    println!("{}", num_monsters);
    map.into_iter().filter(|&c| c == b'#').count() - num_monsters*15
}

pub fn rotate_piece_90(piece: Piece) -> Piece
{
    let mut edges = piece.edges;
    edges.rotate_right(1);
    edges[0] = rev_edge(edges[0]);
    edges[2] = rev_edge(edges[2]);

    let mut edge_keys = piece.edge_keys;
    edge_keys.rotate_right(1);

    let mut data = piece.data;
    rotate_90_clockwise(&mut data, 10);
    Piece { id: piece.id, data, edges: edges, edge_keys }
}

pub fn flip_piece_vertical(piece: Piece) -> Piece
{
    let mut edges = piece.edges;
    let temp = edges[0];
    edges[0] = edges[2];
    edges[1] = rev_edge(edges[1]);
    edges[2] = temp;
    edges[3] = rev_edge(edges[3]);

    let mut edge_keys = piece.edge_keys;
    let temp = edge_keys[0];
    edge_keys[0] = edge_keys[2];
    edge_keys[2] = temp;

    let mut data = piece.data;
    flip_vertical(&mut data, 10);
    Piece { id: piece.id, data, edges: edges, edge_keys }
}

pub fn flip_piece_horizontal(piece: Piece) -> Piece
{
    let mut edges = piece.edges;
    let temp = edges[1];
    edges[0] = rev_edge(edges[0]);
    edges[1] = edges[3];
    edges[2] = rev_edge(edges[2]);
    edges[3] = temp;

    let mut edge_keys = piece.edge_keys;
    let temp = edge_keys[1];
    edge_keys[1] = edge_keys[3];
    edge_keys[3] = temp;

    let mut data = piece.data;
    flip_horizontal(&mut data, 10);
    Piece { id: piece.id, data, edges: edges, edge_keys }
}

pub fn flip_vertical<T>(a: &mut[T], n:usize)
    where T: Copy
{
    for y in 0..n/2
    {
        for x in 0..n
        {
            let temp = a[y*n+x];
            a[y*n+x] = a[(n-y-1)*n+x];
            a[(n-y-1)*n+x] = temp;
        }
    }
}

pub fn flip_horizontal<T>(a: &mut[T], n:usize)
    where T: Copy
{
    for y in 0..n
    {
        for x in 0..n/2
        {
            let temp = a[y*n+x];
            a[y*n+x] = a[y*n+(n-x-1)];
            a[y*n+(n-x-1)] = temp;
        }
    }
}

pub fn rotate_90_clockwise<T>(a: &mut[T], n:usize)
    where T: Copy
{
    for i in 0..n/2
    {
        for j in i..n-i-1
        {
            let temp = a[i*n+j];
            a[i*n+j] = a[(n - 1 - j)*n + i];
            a[(n - 1 - j)*n+i] = a[(n - 1 - i)*n+(n - 1 - j)];
            a[(n - 1 - i)*n+(n - 1 - j)] = a[j*n+(n - 1 - i)];
            a[j*n+(n - 1 - i)] = temp;
        }
    }
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(&parse_input(TEST_DATA)), 20899048083289)
    }

    #[test]
    pub fn part2_test() 
    {
        assert_eq!(part2(&parse_input(TEST_DATA)), 273)
    }
}