pub fn part1(file_data: &str)
{
    let best = file_data.as_bytes().chunks(25 * 6)
        .map(|chunk| 
        {
            let zeros = chunk.iter().filter(|c| **c == '0' as u8).count();
            let ones = chunk.iter().filter(|c| **c == '1' as u8).count();
            let twos = chunk.iter().filter(|c| **c == '2' as u8).count();
            (zeros, ones, twos)
        })
        .min_by(|(zeros_left,_,_), (zeros_right,_,_)| zeros_left.cmp(zeros_right)).unwrap();

        println!("Day 8 part 1: {}", best.1 * best.2);
    
}

pub fn part2(file_data: &str)
{
    let composite : Vec<u8> = file_data.as_bytes().chunks(25 * 6)
        .fold(vec!['2' as u8; 25*6], |comp, layer|
        {
            comp.iter().zip(layer.iter())
                .map(|(c, l)| if *c == '2' as u8 { *l } else { *c })
                .collect()
        });

    println!("Day 8 part 2:");
    composite.chunks(25)
        .for_each(|chunk| println!("{}", chunk.iter().map(|c| match *c as char {'0' => ' ', _ => '*'}).collect::<String>()) )

}