use regex::Regex;
use std::fs;

fn main() {
    let contents = fs::read_to_string("input.txt").expect("Should have been able to read the file");
    // let contents: String = String::from("mul(9,1)");
    let re_p1 = Regex::new(r"mul\(([0-9]+),([0-9]+)\)").unwrap();
    let caps = re_p1.captures_iter(contents.as_str());

    // loop captures
    let mut sum = 0;
    for cap in caps {
        let a: i32 = cap[1].parse().unwrap();
        let b: i32 = cap[2].parse().unwrap();
        sum += a * b;
    }

    println!("part1 = {}", sum);

    // part 2

    let mut mul_enabled = true;

    // match either the mul or the do/don't
    let re_p2 = Regex::new(r"mul\(([0-9]+),([0-9]+)\)|(do\(\))|(don't\(\))").unwrap();
    let caps = re_p2.captures_iter(contents.as_str());

    sum = 0;
    // then enable / disable the mul based on the capture groups
    for cap in caps {
        if cap.get(1).is_some() {
            let a: i32 = cap[1].parse().unwrap();
            let b: i32 = cap[2].parse().unwrap();
            if mul_enabled {
                sum += a * b;
            }
        } else if cap.get(3).is_some() {
            mul_enabled = true;
        } else if cap.get(4).is_some() {
            mul_enabled = false;
        }
    }

    println!("part2 = {}", sum);
}
