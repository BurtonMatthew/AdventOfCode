use std::collections::HashMap;

type Reaction<'a> = (&'a str, i64);
type ReactionMap<'a> = HashMap<&'a str, (i64, Vec<Reaction<'a>>)>;

pub fn part1(file_data: &str)
{
    println!("Day 14 part 1: {}", fuel_ore_req(&parse_reactions(file_data), 1));
}

pub fn part2(file_data: &str)
{
    println!("Day 14 part 2: {}", max_fuel_from_ore(&parse_reactions(file_data), 1000000000000));
}


fn parse_reactions(reactions: &str) -> ReactionMap
{
    fn parse_reactant(reactant: &str) -> Reaction
    {
        let mut parts = reactant.split_whitespace();
        let count = parts.next().unwrap().parse::<i64>().unwrap();
        let element = parts.next().unwrap();
        (element, count)
    }
    let mut reaction_map = HashMap::new();

    for line in reactions.lines()
    {
        let reactants = line.split("=>").collect::<Vec<&str>>();
        let result = parse_reactant(reactants[1].trim());
        let inputs = reactants[0].split(",").map(|r| parse_reactant(r.trim())).collect::<Vec<(&str, i64)>>();
        reaction_map.insert(result.0, (result.1, inputs));
    }

    reaction_map
}

fn fuel_ore_req(reactions: &ReactionMap, fuel: i64) -> i64
{
    let mut ore_req = 0;
    let mut required_elems : HashMap<&str, i64> = HashMap::new();
    let mut spare_elems : HashMap<&str, i64> = HashMap::new();

    required_elems.insert("FUEL", fuel);

    while required_elems.len() > 0
    {
        let requirement = required_elems.iter().next().unwrap();
        let element = *requirement.0;
        let mut amount = *requirement.1;

        required_elems.remove(element);

        if let Some(spare) = spare_elems.get_mut(element)
        {
            if amount <= *spare 
            { 
                *spare -= amount;
                continue;
            }
            else
            {
                amount -= *spare;
                *spare = 0;
            }
        }

        if element == "ORE"
        {
            ore_req += amount;
            continue;
        }

        let reaction = &reactions[element];
        //let num_reactions = (amount / reaction.0) + if amount % reaction.0 > 0 { 1 } else { 0 };
        let num_reactions: i64 = ((amount as f64) / (reaction.0 as f64)).ceil() as i64;

        *spare_elems.entry(element).or_insert(0) += (reaction.0 * num_reactions) - amount;
        for new_requirement in &reaction.1
        {
            *required_elems.entry(new_requirement.0).or_insert(0) += new_requirement.1 * num_reactions;
        }
    }

    ore_req
}

fn max_fuel_from_ore(reactions: &ReactionMap, ore: i64) -> i64
{
    let mut min_bound = 1;
    let mut max_bound = ore;

    while min_bound <= max_bound
    {
        let mid_point = (max_bound + min_bound) / 2;
        let ore_cost = fuel_ore_req(reactions, mid_point);
        if ore_cost == ore
        {
            return mid_point;
        }
        else if ore_cost > ore
        {
            max_bound = mid_point-1;
        }
        else
        {
            min_bound = mid_point + 1;
        }
    }

    max_bound
}

#[test]
fn test_fuel_ore_req()
{
    let test_1 = 
    "10 ORE => 10 A
    1 ORE => 1 B
    7 A, 1 B => 1 C
    7 A, 1 C => 1 D
    7 A, 1 D => 1 E
    7 A, 1 E => 1 FUEL";

    let test_2 = 
    "9 ORE => 2 A
    8 ORE => 3 B
    7 ORE => 5 C
    3 A, 4 B => 1 AB
    5 B, 7 C => 1 BC
    4 C, 1 A => 1 CA
    2 AB, 3 BC, 4 CA => 1 FUEL";

    let test_3 = 
    "157 ORE => 5 NZVS
    165 ORE => 6 DCFZ
    44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
    12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
    179 ORE => 7 PSHF
    177 ORE => 5 HKGWZ
    7 DCFZ, 7 PSHF => 2 XJWVT
    165 ORE => 2 GPVTF
    3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT";

    let test_4 = 
    "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
    17 NVRVD, 3 JNWZP => 8 VPVL
    53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
    22 VJHF, 37 MNCFX => 5 FWMGM
    139 ORE => 4 NVRVD
    144 ORE => 7 JNWZP
    5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
    5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
    145 ORE => 6 MNCFX
    1 NVRVD => 8 CXFTF
    1 VJHF, 6 MNCFX => 4 RFSQX
    176 ORE => 6 VJHF";

    let test_5 = 
    "171 ORE => 8 CNZTR
    7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
    114 ORE => 4 BHXH
    14 VRPVC => 6 BMBT
    6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
    6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
    15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
    13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
    5 BMBT => 4 WPTQ
    189 ORE => 9 KTJDG
    1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
    12 VRPVC, 27 CNZTR => 2 XDBXC
    15 KTJDG, 12 BHXH => 5 XCVML
    3 BHXH, 2 VRPVC => 7 MZWV
    121 ORE => 7 VRPVC
    7 XCVML => 6 RJRHP
    5 BHXH, 4 VRPVC => 5 LTCX";

    assert_eq!(fuel_ore_req(&parse_reactions(test_1), 1), 31);
    assert_eq!(fuel_ore_req(&parse_reactions(test_2), 1), 165);
    assert_eq!(fuel_ore_req(&parse_reactions(test_3), 1), 13312);
    assert_eq!(fuel_ore_req(&parse_reactions(test_4), 1), 180697);
    assert_eq!(fuel_ore_req(&parse_reactions(test_5), 1), 2210736);
}

#[test]
fn test_max_fuel_from_ore()
{
    let test_3 = 
    "157 ORE => 5 NZVS
    165 ORE => 6 DCFZ
    44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
    12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
    179 ORE => 7 PSHF
    177 ORE => 5 HKGWZ
    7 DCFZ, 7 PSHF => 2 XJWVT
    165 ORE => 2 GPVTF
    3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT";

    let test_4 = 
    "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
    17 NVRVD, 3 JNWZP => 8 VPVL
    53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
    22 VJHF, 37 MNCFX => 5 FWMGM
    139 ORE => 4 NVRVD
    144 ORE => 7 JNWZP
    5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
    5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
    145 ORE => 6 MNCFX
    1 NVRVD => 8 CXFTF
    1 VJHF, 6 MNCFX => 4 RFSQX
    176 ORE => 6 VJHF";

    let test_5 = 
    "171 ORE => 8 CNZTR
    7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
    114 ORE => 4 BHXH
    14 VRPVC => 6 BMBT
    6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
    6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
    15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
    13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
    5 BMBT => 4 WPTQ
    189 ORE => 9 KTJDG
    1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
    12 VRPVC, 27 CNZTR => 2 XDBXC
    15 KTJDG, 12 BHXH => 5 XCVML
    3 BHXH, 2 VRPVC => 7 MZWV
    121 ORE => 7 VRPVC
    7 XCVML => 6 RJRHP
    5 BHXH, 4 VRPVC => 5 LTCX";

    assert_eq!(max_fuel_from_ore(&parse_reactions(test_3), 1000000000000), 82892753);
    assert_eq!(max_fuel_from_ore(&parse_reactions(test_4), 1000000000000), 5586022);
    assert_eq!(max_fuel_from_ore(&parse_reactions(test_5), 1000000000000), 460664);
}