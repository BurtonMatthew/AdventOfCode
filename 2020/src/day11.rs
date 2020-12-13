use packed_simd::u8x32;
use packed_simd::FromCast;

const PADDING: usize = 2;
type U8SimdVec = u8x32;
const SIMD_WIDTH: usize = std::mem::size_of::<U8SimdVec>();

pub struct StateData
{
    seats: Vec<u8>,
    floors: Vec<u8>,
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

    let floors = seats.iter().map(|i| i ^ 1).collect();

    StateData{seats, floors, width, height, padded_width}
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
                let mut counts = U8SimdVec::splat(0);
                // Sum up moving windows around our position, these correspond to the 8 neighbours
                counts += U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos-input.padded_width-1)]);
                counts += U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos-input.padded_width)]);
                counts += U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos-input.padded_width+1)]);
                counts += U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos-1)]);
                counts += U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos+1)]);
                counts += U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos+input.padded_width-1)]);
                counts += U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos+input.padded_width)]);
                counts += U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos+input.padded_width+1)]);
                counts.write_to_slice_unaligned(&mut neighbor_counts[simd_range(pos)]);
            }
        }

        // Update seats
        for y in PADDING..PADDING+input.height
        {
            for x in (PADDING..input.width+PADDING).step_by(SIMD_WIDTH)
            {
                let pos = y * input.padded_width + x;
                let seats = U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos)]);
                let counts = U8SimdVec::from_slice_unaligned(&neighbor_counts[simd_range(pos)]);
                let new_seats = seats.eq(U8SimdVec::splat(0))
                                    .select(U8SimdVec::from_cast(counts.eq(U8SimdVec::splat(0)))  //Empty seat: sit if no neighbours
                                          , U8SimdVec::from_cast(counts.lt(U8SimdVec::splat(4)))) //Filled seat: stand if >=4 neighbours
                                    & U8SimdVec::from_slice_unaligned(&input.seats[simd_range(pos)]); //Zero out anything that isn't an actual seat
                new_seats.write_to_slice_unaligned(&mut occupied[simd_range(pos)]);
                modified |= seats.ne(new_seats).any();
            }
        }
    }
    occupied.iter().filter(|&&s| s == 1u8).count()
}

#[aoc(day11, part2)]
pub fn part2(input : &StateData) -> usize
{
    let mut occupied = vec![0; input.seats.len()];
    let mut neighbor_counts = vec![0; input.seats.len()];

    let mut remote_adjacencies: Vec<(usize, usize)> = Vec::with_capacity(input.seats.len() / 2);
    let x_range = PADDING..input.width+PADDING;
    let y_range = PADDING..PADDING+input.height;
    let step_right = 1;
    let step_down = input.padded_width;

    // Build up remote adjacencies (>2 seat away)
    for y in y_range.clone()
    {
        for x in x_range.clone()
        {
            let pos = y * input.padded_width + x;
            if input.seats[pos] == 0 { continue; }

            // scan right
            let mut dist = 1;
            while x + dist < x_range.end && input.seats[pos+step_right*dist] == 0 { dist += 1; }
            if dist > 2 && input.seats[pos+step_right*dist] == 1 { remote_adjacencies.push((pos, pos+step_right*dist)); }

            // scan botleft
            dist = 1;
            while x - dist >= x_range.start && y + dist < y_range.end  && input.seats[pos+step_down*dist-step_right*dist] == 0 { dist += 1; }
            if dist > 2 && input.seats[pos+step_down*dist-step_right*dist] == 1 { remote_adjacencies.push((pos, pos+step_down*dist-step_right*dist)); }

            // scan bot
            dist = 1;
            while y + dist < y_range.end && input.seats[pos+step_down*dist] == 0 { dist += 1; }
            if dist > 2 && input.seats[pos+step_down*dist] == 1 { remote_adjacencies.push((pos, pos+step_down*dist)); }

            // scan botright
            dist = 1;
            while x + dist < x_range.end && y + dist < y_range.end  && input.seats[pos+step_down*dist+step_right*dist] == 0 { dist += 1; }
            if dist > 2 && input.seats[pos+step_down*dist+step_right*dist] == 1 { remote_adjacencies.push((pos, pos+step_down*dist+step_right*dist)); }
        }
    }

    let simd_range = |pos| pos..pos+SIMD_WIDTH;
    let mut modified = true;
    while modified
    {
        modified = false;

        // Count neighbours
        for y in y_range.clone()
        {
            for x in x_range.clone().step_by(SIMD_WIDTH)
            {
                let pos = y * input.padded_width + x;
                let mut counts = U8SimdVec::splat(0);
                // Sum up moving windows around our position, these correspond to a 5x5 convolution
                // Where logic for the right side would be x = occupied[x+1] || occupied[x+2] && floor[x+1]
                counts += U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos-step_down-step_right)])
                    | U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos-step_down*2-step_right*2)]) 
                    &  U8SimdVec::from_slice_unaligned(&input.floors[simd_range(pos-step_down-step_right)]);
                    
                counts += U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos-step_down)])
                    | U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos-step_down*2)]) 
                    &  U8SimdVec::from_slice_unaligned(&input.floors[simd_range(pos-step_down)]);

                counts += U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos-step_down+step_right)])
                    | U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos-step_down*2+step_right*2)]) 
                    &  U8SimdVec::from_slice_unaligned(&input.floors[simd_range(pos-step_down+step_right)]);


                counts += U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos-step_right)])
                    | U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos-step_right*2)]) 
                    &  U8SimdVec::from_slice_unaligned(&input.floors[simd_range(pos-step_right)]);


                counts += U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos+step_right)]) 
                    | U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos+step_right*2)]) 
                    &  U8SimdVec::from_slice_unaligned(&input.floors[simd_range(pos+step_right)]);

                counts += U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos+step_down-step_right)])
                    | U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos+step_down*2-step_right*2)]) 
                    &  U8SimdVec::from_slice_unaligned(&input.floors[simd_range(pos+step_down-step_right)]);

                counts += U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos+step_down)])
                    | U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos+step_down*2)]) 
                    &  U8SimdVec::from_slice_unaligned(&input.floors[simd_range(pos+step_down)]);


                counts += U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos+step_down+step_right)])
                    | U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos+step_down*2+step_right*2)]) 
                    &  U8SimdVec::from_slice_unaligned(&input.floors[simd_range(pos+step_down+step_right)]);


                counts.write_to_slice_unaligned(&mut neighbor_counts[simd_range(pos)]);
            }
        }

        // Check remote pairs
        for (s1,s2) in &remote_adjacencies
        {
            neighbor_counts[*s1] += occupied[*s2];
            neighbor_counts[*s2] += occupied[*s1];
        }

        // Update seats
        for y in y_range.clone()
        {
            for x in x_range.clone().step_by(SIMD_WIDTH)
            {
                let pos = y * input.padded_width + x;
                let seats = U8SimdVec::from_slice_unaligned(&occupied[simd_range(pos)]);
                let counts = U8SimdVec::from_slice_unaligned(&neighbor_counts[simd_range(pos)]);
                let new_seats = seats.eq(U8SimdVec::splat(0))
                                    .select(U8SimdVec::from_cast(counts.eq(U8SimdVec::splat(0)))  //Empty seat: sit if no neighbours
                                        , U8SimdVec::from_cast(counts.lt(U8SimdVec::splat(5)))) //Filled seat: stand if >=5 neighbours
                                    & U8SimdVec::from_slice_unaligned(&input.seats[simd_range(pos)]); //Zero out anything that isn't an actual seat
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
        assert_eq!(part2(&parse_input(TEST_DATA)), 26)
    }
}