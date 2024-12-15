use std::{fs, io::Write};

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
struct Point {
    x: i64,
    y: i64,
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

impl ::core::ops::AddAssign for Point {
    fn add_assign(&mut self, other: Self) {
        *self = Self {
            x: self.x + other.x,
            y: self.y + other.y,
        };
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
struct Robot {
    pos: Point,
    vel: Point,
}

fn line_to_point(l: &str) -> Point {
    // convert p=0,4 to Point { x: 0, y: 4 }
    let mut sp = l.split_terminator(',');

    // first element is offset by 2
    let x = sp
        .next()
        .unwrap()
        .chars()
        .skip(2)
        .collect::<String>()
        .parse::<i64>()
        .unwrap();

    // second element is offset by 0
    let y = sp
        .next()
        .unwrap()
        .chars()
        .collect::<String>()
        .parse::<i64>()
        .unwrap();

    Point { x, y }
}

fn line_to_robot(l: &str) -> Robot {
    let mut sp = l.split_ascii_whitespace();

    let pos = line_to_point(sp.next().unwrap());
    let vel = line_to_point(sp.next().unwrap());

    Robot { pos, vel }
}

type MapSize = Point;
fn parse_input() -> (Vec<Robot>, MapSize) {
    let _s = String::from(
        r"p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3",
    );
    let _s = String::from(r"p=2,4 v=2,-3");

    let s = fs::read_to_string("input.txt").unwrap();

    (
        s.lines().into_iter().map(line_to_robot).collect(),
        Point { x: 101, y: 103 },
        // Point { x: 11, y: 7 },
    )
}

fn update_robot(robot: &mut Robot, map_size: &MapSize) {
    robot.pos += robot.vel;
    // going north, so we must wrap around and
    // enter from the south
    if robot.pos.y < 0 {
        robot.pos.y = map_size.y - robot.pos.y.abs();
    }
    // going south, so we must wrap around and
    // enter from the north
    else if robot.pos.y >= map_size.y {
        robot.pos.y = robot.pos.y % map_size.y;
    }
    // going west, so we must wrap around and
    // enter from the east
    if robot.pos.x < 0 {
        robot.pos.x = map_size.x - robot.pos.x.abs();
    }
    // going east, so we must wrap around and
    // enter from the west
    else if robot.pos.x >= map_size.x {
        robot.pos.x = robot.pos.x % map_size.x;
    }
}

fn update_robots(robots: &mut Vec<Robot>, map_size: &MapSize) {
    for robot in robots {
        update_robot(robot, map_size);
    }
}

fn print_robots(robots: &Vec<Robot>, map_size: &MapSize, _file: &mut std::fs::File, idx: usize) {
    let mut map = vec![vec![' '; map_size.x as usize]; map_size.y as usize];

    for robot in robots {
        // count robots in this pos
        let cnt = robots.iter().filter(|r| r.pos == robot.pos).count();
        map[robot.pos.y as usize][robot.pos.x as usize] = cnt.to_string().chars().next().unwrap();
    }

    for row in map {
        // for c in row.cl {
        // print!("{}", c);

        // write to file
        _file
            .write_all(row.into_iter().collect::<String>().as_bytes())
            .unwrap();
        // }
        // write new line
        _file.write_all("\n".as_bytes()).unwrap();
        // println!();
        // println!("{:?}", row);
    }
    _file
        .write_all(
            format!(
                "\n------------------------------{}-------------------------------------\n",
                idx
            )
            .as_bytes(),
        )
        .unwrap();
}

#[derive(Debug)]
struct Quadrant {
    lower_left: Point,
    upper_right: Point,
}
fn is_robot_in_quadrant(robot: &Robot, quadrant: &Quadrant) -> bool {
    let lower_left = quadrant.lower_left;
    let upper_right = quadrant.upper_right;
    let is_in = (robot.pos.x >= lower_left.x && robot.pos.x < upper_right.x)
        && (robot.pos.y >= lower_left.y && robot.pos.y < upper_right.y);

    if is_in {
        // println!("Robot {:?} is in quadrant {:?}", robot, quadrant);
    }

    is_in
}

fn robots_in_quadrant(robots: &Vec<Robot>, quadrant: &Quadrant) -> usize {
    robots
        .iter()
        .filter(|r| is_robot_in_quadrant(r, quadrant))
        .count()
}

fn calc_final_pos(robots: &Vec<Robot>, map_size: &MapSize) -> usize {
    // NW
    let q1 = Quadrant {
        lower_left: Point { x: 0, y: 0 },
        upper_right: Point {
            x: map_size.x / 2,
            y: map_size.y / 2,
        },
    };

    // NE
    let q2 = Quadrant {
        lower_left: Point {
            x: map_size.x / 2 + 1,
            y: 0,
        },
        upper_right: Point {
            x: map_size.x,
            y: map_size.y / 2,
        },
    };

    // NW
    let q3 = Quadrant {
        lower_left: Point {
            x: 0,
            y: map_size.y / 2 + 1,
        },
        upper_right: Point {
            x: map_size.x / 2,
            y: map_size.y,
        },
    };

    // NE
    let q4 = Quadrant {
        lower_left: Point {
            x: map_size.x / 2 + 1,
            y: map_size.y / 2 + 1,
        },
        upper_right: Point {
            x: map_size.x,
            y: map_size.y,
        },
    };

    robots_in_quadrant(robots, &q1)
        * robots_in_quadrant(robots, &q2)
        * robots_in_quadrant(robots, &q3)
        * robots_in_quadrant(robots, &q4)
}

fn is_bottom_full(robots: &Vec<Robot>, map_size: &MapSize) -> bool {
    robots.iter().filter(|r| r.pos.y == map_size.y - 1).count() > (map_size.x / 5) as usize
}

fn main() {
    let (mut robots, map_size) = parse_input();
    // print_robots(&robots, &map_size);

    // open file for writing
    let mut file = std::fs::File::create("output5.txt").unwrap();

    for i in 0..7000 {
        update_robots(&mut robots, &map_size);
        // if i < 80 {
        //     continue;
        // }
        // // sleep 50 ms
        // std::thread::sleep(std::time::Duration::from_millis(1000));
        // println!("---------------------------------------------------------------------------------------------------------");
        // println!(
        //     "                                        -------{}--------",
        //     i
        // );
        // println!("---------------------------------------------------------------------------------------------------------");
        print_robots(&robots, &map_size, &mut file, i);
    }

    println!("Part 1: {:?}", calc_final_pos(&robots, &map_size));

    /*
    part 2:
    solved by doing a visual inspection of the output file
    it can be seen that there is a "beat" every 101 and 103 iterations
    for to different series (x and y velocities)
    these are both offset by a constant 87 and 112 respectively
    it was then assumed that when these two series overlap
    a message would be displayed. this turned out to be correct.
    the convergence could easily be found by writing out the series and finding the first overlap.
     */
}
