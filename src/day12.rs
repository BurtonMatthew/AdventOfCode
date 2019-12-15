use itertools::Itertools;

pub fn part1(file_data: &str)
{
    let mut meteors_pos : Vec<Vec3> = file_data.chars()
                                .map(|c| if c == '\n' { ',' } else { c })
                                .filter(|c| *c == ',' || *c == '-' || (*c >= '0' && *c <= '9'))
                                .collect::<String>()
                                .split(",")
                                .map(|num| num.parse::<i32>().unwrap())
                                .chunks(3).into_iter()
                                .map(|chunk| chunk.collect::<Vec<i32>>())
                                .map(|chunk| Vec3 {x: chunk[0], y: chunk[1], z: chunk[2]})
                                .collect();

    let num_meteors = meteors_pos.len();
    let mut meteors_vel = vec![Vec3 {x:0, y:0, z:0}; num_meteors];

    for _ in 0..1000
    {
        // apply gravity
        for i in 0..num_meteors-1
        {
            for j in i+1..num_meteors
            {
                if meteors_pos[i].x < meteors_pos[j].x { meteors_vel[i].x += 1; meteors_vel[j].x -= 1 }
                else if meteors_pos[i].x > meteors_pos[j].x { meteors_vel[i].x -= 1; meteors_vel[j].x += 1 }
                if meteors_pos[i].y < meteors_pos[j].y { meteors_vel[i].y += 1; meteors_vel[j].y -= 1 }
                else if meteors_pos[i].y > meteors_pos[j].y { meteors_vel[i].y -= 1; meteors_vel[j].y += 1 }
                if meteors_pos[i].z < meteors_pos[j].z { meteors_vel[i].z += 1; meteors_vel[j].z -= 1 }
                else if meteors_pos[i].z > meteors_pos[j].z { meteors_vel[i].z -= 1; meteors_vel[j].z += 1 }
            }
        }

        // do velocity
        for i in 0..num_meteors
        {
            meteors_pos[i] += meteors_vel[i];
        }
    }

    let energy = (0..num_meteors).fold(0, |s, i| 
        {
            let potential = meteors_pos[i].x.abs() + meteors_pos[i].y.abs() + meteors_pos[i].z.abs();
            let kinetic = meteors_vel[i].x.abs() + meteors_vel[i].y.abs() + meteors_vel[i].z.abs();
            s + (potential * kinetic)
        });

    println!("Day 12 part 1: {}", energy);
}

pub fn part2(file_data: &str)
{
    let meteors_start_pos : Vec<Vec3> = file_data.chars()
                                .map(|c| if c == '\n' { ',' } else { c })
                                .filter(|c| *c == ',' || *c == '-' || (*c >= '0' && *c <= '9'))
                                .collect::<String>()
                                .split(",")
                                .map(|num| num.parse::<i32>().unwrap())
                                .chunks(3).into_iter()
                                .map(|chunk| chunk.collect::<Vec<i32>>())
                                .map(|chunk| Vec3 {x: chunk[0], y: chunk[1], z: chunk[2]})
                                .collect();

    let mut meteors_pos = meteors_start_pos.clone();
    let num_meteors = meteors_pos.len();
    let mut meteors_vel = vec![Vec3::zero(); num_meteors];
    let mut num_cycles_found = 0;
    let mut axis_periods : Vec<i64> = vec![0; 3];
    let mut cycle = 0;

    while num_cycles_found < 3
    {
        cycle += 1;
        // apply gravity
        for i in 0..num_meteors-1
        {
            for j in i+1..num_meteors
            {
                if meteors_pos[i].x < meteors_pos[j].x { meteors_vel[i].x += 1; meteors_vel[j].x -= 1 }
                else if meteors_pos[i].x > meteors_pos[j].x { meteors_vel[i].x -= 1; meteors_vel[j].x += 1 }
                if meteors_pos[i].y < meteors_pos[j].y { meteors_vel[i].y += 1; meteors_vel[j].y -= 1 }
                else if meteors_pos[i].y > meteors_pos[j].y { meteors_vel[i].y -= 1; meteors_vel[j].y += 1 }
                if meteors_pos[i].z < meteors_pos[j].z { meteors_vel[i].z += 1; meteors_vel[j].z -= 1 }
                else if meteors_pos[i].z > meteors_pos[j].z { meteors_vel[i].z -= 1; meteors_vel[j].z += 1 }
            }
        }

        // do velocity
        for i in 0..num_meteors
        {
            meteors_pos[i] += meteors_vel[i];
        }

        // check cycles
        for i in 0..3
        {
            if axis_periods[i] == 0 
                && meteors_pos.iter().zip(meteors_start_pos.iter()).fold(true, |s, (a,b)| s && a[i] == b[i]) 
                && meteors_vel.iter().fold(true, |s, v| s && v[i] == 0)
            {
                axis_periods[i] = cycle;
                num_cycles_found += 1;
            }
        }
    }

    let total_period = axis_periods.iter().fold(1, |a, b| (a * b) / gcd(a, *b));

    println!("Day 12 part 2: {}", total_period);
}

#[derive(Clone, Copy, Eq, PartialEq)]
struct Vec3
{
    x: i32,
    y: i32,
    z: i32,
}

impl Vec3
{
    fn zero() -> Self
    {
        Self { x:0, y:0, z:0 }
    }
}

impl std::ops::Add for Vec3
{
    type Output = Self;

    fn add(self, other: Self) -> Self
    {
        Self 
        { 
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        }
    }
}

impl std::ops::AddAssign for Vec3
{
    fn add_assign(&mut self, other: Self)
    {
        *self = other + *self;
    }
}

impl std::ops::Index<usize> for Vec3
{
    type Output = i32;
    fn index(&self, index:usize) -> &Self::Output
    {
        match index
        {
            0 => &self.x,
            1 => &self.y,
            2 => &self.z,
            _ => panic!("Invalid index to Vec3")
        }
    }
}

fn gcd<T>(mut a: T, mut b: T) -> T 
    where T : num::Integer + Copy,
{
    while b != T::zero()
    {
        let tmp = a;
        a = b;
        b = tmp % b;
    }
    a
}