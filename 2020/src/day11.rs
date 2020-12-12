use packed_simd::u8x64;

const PADDING: usize = 1;
const SIMD_WIDTH: usize = 64;

pub struct StateData
{
    seats: Vec<u8>,
    width: usize,
    height: usize,
    padded_width: usize,
}

#[aoc_generator(day11)]
pub fn parse_input(buf :&str) -> StateData
{
    let width = buf.find('\n').unwrap();
    let height = buf.lines().count();
    let padded_width = ((width + SIMD_WIDTH -1) / SIMD_WIDTH) * SIMD_WIDTH + PADDING + PADDING;
    let padded_height = height + PADDING + PADDING;
    let vec_len = padded_width * padded_height;

    let mut seats = Vec::with_capacity(vec_len);

    for _ in 0..padded_width*PADDING
    {
        seats.push(0);
    }

    let mut i = 0;
    let mut bytes = buf.bytes().filter(|&c| c != b'\n');
    for _ in 0..height
    {
        for _ in 0..PADDING
        {
            seats.push(0);
        }
        for _ in 0..width
        {
            if bytes.next().unwrap() == b'L' { seats.push(1); } else { seats.push(0); }
        }
        for _ in 0..padded_width-width-PADDING
        {
            seats.push(0);
        }
    }

    for _ in 0..padded_width*PADDING
    {
        seats.push(0);
    }

    StateData{seats, width, height, padded_width}
}

#[aoc(day11, part1)]
pub fn part1(input : &StateData) -> usize
{
    let mut occupied = vec![0; input.seats.len()];
    let mut neighbor_counts = vec![0; input.seats.len()];

    let simd_range = |pos| pos..pos+SIMD_WIDTH;

    let mut modified = true;

    while modified
    {
        modified = false;

        // Count neighbours
        for y in PADDING..PADDING+input.height
        {
            for x in (PADDING..input.width+PADDING).step_by(SIMD_WIDTH)
            {
                let pos = y * input.padded_width + x;
                let mut counts = u8x64::splat(0);
                // Sum up moving windows around our position, these correspond to the 8 neighbours
                counts += u8x64::from_slice_unaligned(&occupied[simd_range(pos-input.padded_width-1)]);
                counts += u8x64::from_slice_unaligned(&occupied[simd_range(pos-input.padded_width)]);
                counts += u8x64::from_slice_unaligned(&occupied[simd_range(pos-input.padded_width+1)]);
                counts += u8x64::from_slice_unaligned(&occupied[simd_range(pos-1)]);
                counts += u8x64::from_slice_unaligned(&occupied[simd_range(pos+1)]);
                counts += u8x64::from_slice_unaligned(&occupied[simd_range(pos+input.padded_width-1)]);
                counts += u8x64::from_slice_unaligned(&occupied[simd_range(pos+input.padded_width)]);
                counts += u8x64::from_slice_unaligned(&occupied[simd_range(pos+input.padded_width+1)]);
                counts.write_to_slice_unaligned(&mut neighbor_counts[simd_range(pos)]);
            }
        }

        // Update seats
        for y in PADDING..PADDING+input.height
        {
            for x in (PADDING..input.width+PADDING).step_by(SIMD_WIDTH)
            {
                let pos = y * input.padded_width + x;
                let seats = u8x64::from_slice_unaligned(&occupied[simd_range(pos)]);
                let counts = u8x64::from_slice_unaligned(&neighbor_counts[simd_range(pos)]);
                let new_seats = seats.eq(u8x64::splat(0))
                                    .select(counts.eq(u8x64::splat(0)).select(u8x64::splat(1), u8x64::splat(0))  //Empty seat: sit if no neighbours
                                          , counts.ge(u8x64::splat(4)).select(u8x64::splat(0), u8x64::splat(1))) //Filled seat: stand if >=4 neighbours
                                    & u8x64::from_slice_unaligned(&input.seats[simd_range(pos)]); //Zero out anything that isn't an actual seat
                new_seats.write_to_slice_unaligned(&mut occupied[simd_range(pos)]);
                modified |= seats.ne(new_seats).any();
            }
        }
    }
    occupied.iter().filter(|&&s| s == 1u8).count()
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(&parse_input(TEST_DATA)), 37)
    }

    #[test]
    pub fn part2_test() 
    {
        //assert_eq!(part2(&parse_input(TEST_DATA)), 26)
    }
}