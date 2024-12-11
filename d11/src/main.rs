use std::{collections::HashMap, fs};

type Stones = Vec<u64>;

fn parse_input() -> Stones {
    let s = fs::read_to_string("input.txt").expect("Should have been able to read the file");
    // let s = String::from(r"125 17");

    s.split_ascii_whitespace()
        .map(|stone| stone.parse::<u64>().unwrap())
        .collect()
}

fn transform_stone(stone: u64) -> Stones {
    if stone == 0 {
        return vec![1];
    }

    let mut stone_1 = stone.to_string();
    if stone_1.len() % 2 == 0 {
        let stone_2 = stone_1.split_off(stone_1.len() / 2);
        return vec![
            stone_1.parse::<u64>().unwrap(),
            stone_2.parse::<u64>().unwrap(),
        ];
    } else {
        return vec![stone * 2024];
    }
}

type Precomputed = HashMap<(u64, u32), u64>;

fn expand_stone(stone: u64, depth: u32, max_depth: u32, precomputed: &mut Precomputed) -> u64 {
    let depth = depth + 1;

    if depth == max_depth {
        // println!("Final stone: {}", stone);
        return 1; // done expanding
    }

    let current = (stone, depth);
    if let Some(val) = precomputed.get(&current) {
        // println!("Found precomputed val!");
        return *val;
    }

    if stone == 0 {
        let res = expand_stone(1, depth, max_depth, precomputed);
        precomputed.insert(current, res);
        return res;
    }

    let res: u64;
    let mut stone_1 = stone.to_string();
    if stone_1.len() % 2 == 0 {
        let stone_2 = stone_1.split_off(stone_1.len() / 2);
        let stone_1 = stone_1.parse::<u64>().unwrap();
        let stone_2 = stone_2.parse::<u64>().unwrap();
        // println!("Expanding {} to {} and {}", stone, stone_1, stone_2);
        res = expand_stone(stone_1, depth, max_depth, precomputed)
            + expand_stone(stone_2, depth, max_depth, precomputed);
    } else {
        res = expand_stone(stone * 2024, depth, max_depth, precomputed);
    }

    // cache result for future reuse
    precomputed.insert(current, res);

    res
}

fn blink(stones: &Stones) -> Stones {
    stones
        .into_iter()
        .flat_map(|stone| transform_stone(*stone))
        .collect()
}

// let this part 1 stand as a historic relic for all to behold and be warned of
// the inefficient solutions that are produced by the lazy mind.
fn part_1() {
    let mut stones = parse_input();
    println!("{:?}", stones);

    for i in 0..25 {
        let prev = stones.len() as f64;
        stones = blink(&stones);
        let k = stones.len() as f64 / prev;
        println!("{:?} = {:?} ({})", i, stones.len(), k);
    }

    println!("{:?}", stones.len());
}

fn part_2() {
    let stones = parse_input();
    println!("{:?}", stones);

    let mut precomputed: Precomputed = HashMap::new();
    let total: u64 = stones
        .iter()
        .map(|stone| expand_stone(*stone, 0, 76, &mut precomputed))
        .sum();

    println!("part 2 = {total}");
    //   let mut stones = parse_input();
    println!("precomputed entries {}", precomputed.len());
}

fn main() {
    part_1();
    part_2();
}
