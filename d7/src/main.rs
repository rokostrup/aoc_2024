use std::fs;

#[derive(Debug)]
struct ResWithArgs {
    res: u64,
    args: Vec<u64>,
}

type TestBook = Vec<ResWithArgs>;

fn parse_line(l: &str) -> ResWithArgs {
    let mut r = ResWithArgs {
        res: 0,
        args: Vec::new(),
    };

    for s in l.split_ascii_whitespace() {
        if s.contains(':') {
            r.res = s.trim_matches(':').parse().unwrap();
        } else {
            r.args.push(s.parse().unwrap());
        }
    }
    r
}

fn parse_input() -> TestBook {
    let s = fs::read_to_string("input.txt").expect("Should have been able to read the file");
    let _s = String::from(
        r"190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20",
    );

    s.split_terminator('\n').map(parse_line).collect()
}

fn can_results_work(rwa: &ResWithArgs) -> bool {
    let mut combs = vec![0 as u64];
    for arg in rwa.args.iter() {
        // take the current number and combine it with all
        combs = combs
            .iter()
            .flat_map(|x| vec![x * arg, x + arg])
            .filter(|x| *x > 0)
            .collect();
    }

    combs.contains(&rwa.res)
}

fn main() {
    let book = parse_input();
    // println!("{book:?}");
    let total: u64 = book
        .iter()
        .filter(|x| can_results_work(x))
        .map(|x| x.res)
        .sum();

    println!("{total}");
}
