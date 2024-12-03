use regex::Regex;
use std::fs;

fn main() {
    let contents = fs::read_to_string("input.txt").expect("Should have been able to read the file");
    // let contents: String = String::from("mul(9,1)");
    let re = Regex::new(r"mul\(([0-9]+),([0-9]+)\)").unwrap();

    let caps = re.captures_iter(contents.as_str());

    // loop captures
    let mut sum = 0;
    for cap in caps {
        let a: i32 = cap[1].parse().unwrap();
        let b: i32 = cap[2].parse().unwrap();
        sum += a * b;
    }

    println!("part1 = {}", sum);
}
