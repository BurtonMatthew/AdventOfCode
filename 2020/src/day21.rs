use std::collections::HashSet;
use std::collections::HashMap;
use std::iter::FromIterator;
use itertools::Itertools;

pub fn parse_input(buf :&str) -> (HashMap<&str, usize>, HashMap<&str, HashSet<&str>>)
{
    let mut ingredient_counts = HashMap::new();
    let mut allergens_to_ingredients = HashMap::new();
    for line in buf.lines()
    {
        let mut parts = line.split(" (contains ");
        let ingredient_part = parts.next().unwrap();
        let allergen_part = parts.next().unwrap();
        let ingredients = HashSet::from_iter(ingredient_part.split(" "));
        ingredients.iter().for_each(|&ingredient| *ingredient_counts.entry(ingredient).or_insert(0) += 1 );
        for allergen in allergen_part.split(" ")
        {
            let allergen = &allergen[0..allergen.len()-1];
            if allergens_to_ingredients.contains_key(allergen)
            {
                allergens_to_ingredients.insert(allergen, allergens_to_ingredients.get(allergen).unwrap() & &ingredients);
            }
            else
            {
                allergens_to_ingredients.insert(allergen, ingredients.clone());
            }
        }
    }
    (ingredient_counts, allergens_to_ingredients)
}

#[aoc(day21, part1)]
pub fn part1(input : &str) -> usize
{
    let (ingredient_counts, allergen_map) = parse_input(input);
    let dangerous = allergen_map.into_iter().flat_map(|(_,ingredients)| ingredients).collect::<HashSet<_>>();
    ingredient_counts.into_iter()
                     .map(|(ingredient, count)| if dangerous.contains(ingredient) {0} else {count})
                     .sum()
}

#[aoc(day21, part2)]
pub fn part2(input : &str) -> String
{
    let (_, mut allergen_map) = parse_input(input);
    let mut allergens = Vec::with_capacity(allergen_map.len());

    while !allergen_map.is_empty()
    {
        for (allergen, ingredients) in allergen_map.iter() 
        {
            if ingredients.len() == 1 
            {
                let ingredient = ingredients.iter().next().unwrap();
                allergens.push((*allergen, *ingredient));
                break;
            }
        }

        let (allergen, ingredient) = allergens.last().unwrap();
        allergen_map.remove(allergen);

        for (_, ingredients) in allergen_map.iter_mut() 
        {
            ingredients.remove(ingredient);
        }
    }

    allergens.into_iter()
             .sorted_by(|(allergen1, _), (allergen2, _)| allergen1.cmp(allergen2) )
             .map(|(_, ingredient)| ingredient)
             .intersperse(",")
             .collect::<String>()
}

#[cfg(test)]
mod tests 
{
    use super::*;

    const TEST_DATA: &str = 
"mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)";

    #[test]
    pub fn part1_test() 
    {
        assert_eq!(part1(TEST_DATA), 5)
    }

    #[test]
    pub fn part2_test() 
    {
        assert_eq!(part2(TEST_DATA), "mxmxvkd,sqjhc,fvjkl")
    }
}