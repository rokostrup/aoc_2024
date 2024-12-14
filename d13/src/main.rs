use std::fs;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
struct Point {
    x: i128,
    y: i128,
}

impl ::core::ops::Add for Point {
    type Output = Point;
    fn add(self, rhs: Point) -> Point {
        Point {
            x: self.x.add(rhs.x),
            y: self.y.add(rhs.y),
        }
    }
}

impl ::core::ops::Sub for Point {
    type Output = Point;
    fn sub(self, rhs: Point) -> Point {
        Point {
            x: self.x.sub(rhs.x),
            y: self.y.sub(rhs.y),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
struct Claw {
    a: Point,
    b: Point,
}

type Prize = Point;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
struct Problem {
    claw: Claw,
    prize: Prize,
}

fn line_to_point(l: &str, offset: i128) -> Point {
    let sp = l.split_ascii_whitespace();

    let mut x = 0;
    let mut y = 0;

    for s in sp {
        if s.starts_with("X") {
            // find ,
            let i = s.find(',');
            if i.is_none() {
                x = s[2..].parse::<i128>().unwrap();
            } else {
                x = s[2..i.unwrap()].parse::<i128>().unwrap();
            }
        } else if s.starts_with("Y") {
            y = s[2..].parse::<i128>().unwrap();
        }
    }

    Point {
        x: x + offset,
        y: y + offset,
    }
}

fn to_problem(l1: &str, l2: &str, l3: &str) -> Problem {
    Problem {
        claw: Claw {
            a: line_to_point(l1, 0),
            b: line_to_point(l2, 0),
        },
        prize: line_to_point(l3, 10000000000000),
    }
}

fn parse_input() -> Vec<Problem> {
    let _s = String::from(
        r"Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279",
    );

    let s = fs::read_to_string("input.txt").expect("Should have been able to read the file");

    let mut ls = s.lines();
    let mut v = Vec::new();

    loop {
        let l1 = ls.next().unwrap();
        let l2 = ls.next().unwrap();
        let l3 = ls.next().unwrap();

        v.push(to_problem(l1, l2, l3));

        if ls.next().is_none() {
            break;
        }
    }

    v
}

fn cost(sol: &Point) -> usize {
    sol.x as usize * 3 + sol.y as usize
}

type Solutions = Vec<Point>;
fn find_cheapest(sols: &Solutions) -> usize {
    sols.into_iter()
        .map(|p| cost(p))
        // .min_by(|s1, s2| cost(s1).cmp(&cost(*s2)))
        .min()
        .unwrap() as usize
}

/*
example of solving analytically:

sol for b:
b = (c1*x0 - x1*c0) / (y1 * x0 - y0 * x1)

sol for a:
a = (c0 - b*y0) / x0

testing with
x0 = 94
y0 = 22
x1 = 34
y1 = 67
c0 = 8400
c1 = 5400
*/
fn math_solver(x0: i128, y0: i128, x1: i128, y1: i128, c0: i128, c1: i128) -> Option<(i128, i128)> {
    let denom = y1 * x0 - y0 * x1;
    let nom = c1 * x0 - x1 * c0;
    let b = (c1 * x0 - x1 * c0) / (y1 * x0 - y0 * x1);
    let a = (c0 - b * y0) / x0;

    if c0 != a * x0 + b * y0 {
        return None;
    }
    if c1 != a * x1 + b * y1 {
        return None;
    }

    Some((a, b))
}

fn solve(p: &Problem) -> Solutions {
    let mut v: Solutions = Vec::new();

    if let Some(sol) = math_solver(
        p.claw.a.x, p.claw.b.x, p.claw.a.y, p.claw.b.y, p.prize.x, p.prize.y,
    ) {
        v.push(Point { x: sol.0, y: sol.1 });
    }
    v
}

fn main() {
    // set offset in parse input to 0 for part 1
    // set offset to 10000000000000 for part 2
    let problems = parse_input();

    let cheapest: usize = problems
        .iter()
        .map(|prob| solve(prob))
        .filter(|sols| !sols.is_empty())
        .map(|sols| find_cheapest(&sols))
        .sum();

    println!("Solution = {:?}", cheapest);
}
