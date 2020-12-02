use intcode::Program;
use itertools::Itertools;

pub fn part1(file_data: &str)
{
    let proto_prog: Program = file_data.parse().unwrap();

    let amplifier = |phase, signal| 
                    { 
                        let mut prog = proto_prog.clone();
                        prog.push_input(phase);
                        prog.push_input(signal);
                        prog.next().unwrap()
                    };

    let result = (0..5).permutations(5)
        .map(|v| 
            { 
                let mut signal = 0;
                for phase in v
                {
                    signal = amplifier(phase, signal);
                }
                signal
            }).max().unwrap();

    println!("Day 7 Part 1: {}", result);
}

pub fn part2(file_data: &str)
{
    let proto_prog: Program = file_data.parse().unwrap();

    const NUM_AMPS : usize = 5;

    let result = (5..10).permutations(NUM_AMPS)
        .map(|perm|
    {
        let mut amps = vec![proto_prog.clone(); NUM_AMPS];
        for i in 0..NUM_AMPS
        {
            amps[i].push_input(perm[i]);
        }
        amps[0].push_input(0);

        let mut latest_output = 0;
        let mut next_amp = 0;
        while let Some(cur_output) = amps[next_amp].next()
        {
            if next_amp == NUM_AMPS -1
            {
                next_amp = 0;
                latest_output = cur_output;
            }
            else
            {
                next_amp += 1;
            }

            amps[next_amp].push_input(cur_output);
        }
        latest_output
    }).max().unwrap();

    println!("Day 7 Part 2: {}", result);
}