use std::borrow::Borrow;
use std::clone;
use std::collections::HashSet;
use std::ops::Add;
use std::{fs, iter::Map};

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
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

// a vector off all the '#' on the map
type Obstructions = Vec<Point>;
type Direction = Point;
type MapSize = Point;

#[derive(Debug)]
struct Guard {
    pos: Point,
    dir: Direction,
    obs: Obstructions,
    m_sz: MapSize,
}

impl Guard {
    fn walk(&mut self) -> bool {
        let new_pos = self.pos + self.dir;

        if new_pos.x >= self.m_sz.x || new_pos.y >= self.m_sz.y {
            println!("Left map at {:?}", new_pos);
            return false; // left the map
        }

        // still on the map - hit an obstacle?
        if self.obs.contains(&new_pos) {
            println!("Hit obstruction at {:?}", new_pos);
            self.rotate_right();
            return self.walk();
        }

        // not hit - update position
        println!("Walkted to {:?}", new_pos);
        self.pos = new_pos;

        true
    }

    fn rotate_right(&mut self) {
        self.dir = match self.dir {
            Point { x: 1, y: 0 } => Point { x: 0, y: 1 },
            Point { x: 0, y: 1 } => Point { x: -1, y: 0 },
            Point { x: -1, y: 0 } => Point { x: 0, y: -1 },
            Point { x: 0, y: -1 } => Point { x: 1, y: 0 },
            _ => {
                panic!("Unexpected turn of events");
            }
        }
    }
}

fn create_obs_from_str(row: usize, c_str: &str) -> Obstructions {
    c_str
        .char_indices()
        .filter(|(_, c)| *c == '#')
        .map(|(col, _)| Point {
            x: col as i32,
            y: row as i32,
        })
        .collect()
}

fn parse_input() -> (Guard) {
    let s = fs::read_to_string("input.txt").expect("Should have been able to read the file");
    //     let s = String::from(
    //         r"....#.....
    // .........#
    // ..........
    // ..#.......
    // .......#..
    // ..........
    // .#..^.....
    // ........#.
    // #.........
    // ......#...",
    //     );

    // we extract all the positions of blocks
    let lines = s.split_ascii_whitespace();

    let cols = lines.clone().into_iter().next().map_or(0, |r| r.len());
    let rows = lines.clone().into_iter().count();
    let obs = lines
        .clone()
        .enumerate()
        .flat_map(|(c, y)| create_obs_from_str(c, y))
        .collect();

    let g_pos = lines
        .into_iter()
        .enumerate()
        .find(|(_, row)| row.contains('^'))
        .map_or(Point { x: 0, y: 0 }, |(row_idx, row)| Point {
            x: row.find('^').unwrap() as i32,
            y: row_idx as i32,
        });

    // build the guard
    Guard {
        pos: g_pos,
        dir: Point { x: 0, y: -1 }, // pointing up (by inspection)
        obs: obs,
        m_sz: Point {
            x: cols as i32,
            y: rows as i32,
        },
    }
}
fn main() {
    let mut g = parse_input();

    let mut visited = HashSet::new();
    visited.insert(g.pos); // start position

    while g.walk() {
        visited.insert(g.pos);
    }

    println!("{}", visited.len());

    println!("Map size = {:?}", g.m_sz);
}
