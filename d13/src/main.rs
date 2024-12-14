use std::fs;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
struct Point {
    x: i32,
    y: i32,
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

fn line_to_point(l: &str) -> Point {
    let sp = l.split_ascii_whitespace();

    let mut x = 0;
    let mut y = 0;

    for s in sp {
        if s.starts_with("X") {
            // find ,
            let i = s.find(',');
            if i.is_none() {
                x = s[2..].parse::<i32>().unwrap();
            } else {
                x = s[2..i.unwrap()].parse::<i32>().unwrap();
            }
        } else if s.starts_with("Y") {
            y = s[2..].parse::<i32>().unwrap();
        }
    }

    Point { x, y }
}

fn to_problem(l1: &str, l2: &str, l3: &str) -> Problem {
    Problem {
        claw: Claw {
            a: line_to_point(l1),
            b: line_to_point(l2),
        },
        prize: line_to_point(l3),
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

fn surpassed(prize: &Prize, pos_sol: &Point) -> bool {
    pos_sol.x > prize.x || pos_sol.y > prize.y
}

// fn solved(prize: Prize, )
fn push_button(pushes: i32, button: &Point) -> Point {
    Point {
        x: button.x * pushes,
        y: button.y * pushes,
    }
}

fn can_hit_perfectly(prize: &Prize, button: &Point, offset: &Point) -> Option<i32> {
    let prize = *prize - *offset;

    if prize.x % button.x != 0 {
        return None;
    }
    if prize.y % button.y != 0 {
        return None;
    }

    let divy = prize.y / button.y;
    let divx = prize.x / button.x;

    if divx != divy {
        return None;
    }

    return Some(divy);
}

type Solutions = Vec<Point>;
fn solve(p: &Problem) -> Solutions {
    let mut v: Solutions = Vec::new();

    // for each push of a, we see how many pushes it takes
    // to either hit or get over the price point

    let mut a = 0;
    let claw = p.claw;
    let prize = p.prize;
    loop {
        let pos_sol = push_button(a, &claw.a);

        // now we check if we surpassed the limit
        if surpassed(&prize, &pos_sol) {
            break; // no more solutions to look for
        }

        // we pushed a times - and did not surpass the prize
        // can we find a perfect solution for y?
        if let Some(b) = can_hit_perfectly(&prize, &claw.b, &pos_sol) {
            v.push(Point { x: a, y: b });
        }

        a += 1;
    }

    v
}

fn cost(sol: &Point) -> usize {
    sol.x as usize * 3 + sol.y as usize
}

fn find_cheapest(sols: &Solutions) -> usize {
    sols.into_iter()
        .map(|p| cost(p))
        // .min_by(|s1, s2| cost(s1).cmp(&cost(*s2)))
        .min()
        .unwrap() as usize
}

fn main() {
    let problems = parse_input();

    // let sol = solve(&prob);

    // println!("Sol = {:?}", sol);
    let cheapest: usize = problems
        .iter()
        .map(|prob| solve(prob))
        .filter(|sols| !sols.is_empty())
        .map(|sols| find_cheapest(&sols))
        .sum();

    // for prob in problems {
    //     println!("Input = {:?}", prob);
    //     let sol = solve(&prob);
    //     println!("Solve = {:?}", sol);
    // }
    println!("Cost {:?}", cheapest);
}
