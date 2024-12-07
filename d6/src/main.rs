use std::collections::HashSet;
use std::fs;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
struct Point {
    x: i32,
    y: i32,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
struct Arrow {
    p: Point,
    dir: Point,
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

#[derive(Debug, Clone)]
struct Guard {
    pos: Point,
    dir: Direction,
    obs: Obstructions,
    m_sz: MapSize,
    new_ob: Option<Point>,
    vis_w_dir: HashSet<Arrow>,
}

enum WalkResult {
    Exit,
    LoopDetected,
    TookStep,
}

impl Guard {
    fn walk(&mut self) -> WalkResult {
        let new_pos = self.pos + self.dir;

        if new_pos.x >= self.m_sz.x || new_pos.y >= self.m_sz.y || new_pos.x < 0 || new_pos.y < 0 {
            return WalkResult::Exit;
        }

        let mut new_hit = false;
        if let Some(p) = self.new_ob {
            new_hit = p == new_pos;
        }

        // still on the map - hit an obstacle?
        if new_hit || self.obs.contains(&new_pos) {
            self.rotate_right();
            return self.walk();
        }

        self.pos = new_pos;

        // cache visited places (with a direction!) for faster loop detection
        if !self.vis_w_dir.insert(Arrow {
            p: new_pos,
            dir: self.dir,
        }) {
            return WalkResult::LoopDetected;
        }

        return WalkResult::TookStep;
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

fn parse_input() -> Guard {
    let s = fs::read_to_string("input.txt").expect("Should have been able to read the file");

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
        new_ob: None,
        vis_w_dir: HashSet::new(),
    }
}

// loop detection - cache guards position and direction
// if already present, loop detected -> abort
fn loop_detected(p: Point, mut g: Guard) -> bool {
    // add new position to check
    g.new_ob = Some(p);
    g.vis_w_dir.clear();

    loop {
        match g.walk() {
            WalkResult::Exit => return false,
            WalkResult::TookStep => continue,
            WalkResult::LoopDetected => return true,
        }
    }
}

// using the following optimzation tricks
// object must be where:
// the guards original path
fn cnt_loop_locations(candidates: &HashSet<Point>, g: &mut Guard) -> u32 {
    candidates
        .iter()
        .filter(|p| loop_detected(**p, g.clone()))
        .count() as u32
}

// compile with optimization!
fn main() {
    let mut g = parse_input();
    let mut g_copy = g.clone();

    let mut visited = HashSet::new();

    loop {
        visited.insert(g.pos);
        match g.walk() {
            WalkResult::Exit => break,
            WalkResult::TookStep => continue,
            WalkResult::LoopDetected => panic!("Should not happen here!"),
        }
    }

    println!("p1 = {}", visited.len());
    // the start position is not a candidate - remove
    visited.remove(&g_copy.pos);
    println!("p2 = {}", cnt_loop_locations(&visited, &mut g_copy));
}
