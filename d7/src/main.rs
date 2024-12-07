use itertools::Itertools;
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

    s.split_terminator('\n').map(parse_line).collect()
}

fn can_res_work(rwa: &ResWithArgs, f: fn(&u64, &u64) -> Vec<u64>) -> bool {
    let mut combs = vec![0 as u64];
    for arg in rwa.args.iter() {
        // take the current number and combine it with all
        combs = combs
            .iter()
            .flat_map(|x| f(x, arg))
            .filter(|x| *x > 0)
            .dedup()
            .collect();
    }

    combs.contains(&rwa.res)
}

// takes in a function, f, that expands the
// current number into a new branch for each operator
fn sum_working_res_by_op(book: &TestBook, f: fn(&u64, &u64) -> Vec<u64>) -> u64 {
    book.iter()
        .filter(|x| can_res_work(x, f))
        .map(|x| x.res)
        .sum()
}

fn part_1(book: &TestBook) -> () {
    let total = sum_working_res_by_op(book, |x, y| vec![x * y, x + y]);
    println!("p1 = {total}");
}

fn concat_numbers(lhs: u64, rhs: u64) -> u64 {
    // we note that concatenating dec numbers is like
    // a shift in decimal (i.e., 10 multiplication)
    // and an add. we therefore need to find the
    // number of digits, n, in the rhs number
    // and mulitply this lhs number with 10^(n+1)

    let exp = (rhs as f64).log10().floor() as u32 + 1;
    lhs * (10 as u64).pow(exp) + rhs
}

fn part_2(book: &TestBook) -> () {
    let total = sum_working_res_by_op(book, |x, y| vec![x * y, x + y, concat_numbers(*x, *y)]);
    println!("p2 = {total}");
}

fn main() {
    let book = parse_input();
    part_1(&book);
    part_2(&book);
}
