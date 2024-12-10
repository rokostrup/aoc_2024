use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
struct Point {
    x: i32,
    y: i32,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
struct TopoPoint {
    p: Point,
    height: u32,
}

#[derive(Debug, Clone)]
struct Walk {
    pos: TopoPoint,
    visited: HashSet<Point>,
}

type TopoRow = Vec<Option<TopoPoint>>;
type TopoMap = Vec<TopoRow>;

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

fn line_to_topo_row(l: &&str, row: usize) -> TopoRow {
    l.char_indices()
        .map(|(col, c)| {
            if c.is_digit(10) {
                Some(TopoPoint {
                    p: Point {
                        x: col as i32,
                        y: row as i32,
                    },
                    height: c.to_digit(10).unwrap(),
                })
            } else {
                None
            }
        })
        .collect()
}

fn create_walks(start_points: &Vec<TopoPoint>) -> Vec<Walk> {
    start_points
        .iter()
        .map(|tp| Walk {
            pos: *tp,
            visited: HashSet::new(),
        })
        .collect()
}

fn parse_input() -> TopoMap {
    let s = fs::read_to_string("input.txt").expect("Should have been able to read the file");
    //     let s = String::from(
    //         r"...0...
    // ...1...
    // ...2...
    // 6543456
    // 7.....7
    // 8.....8
    // 9.....9",
    //     );
    // let s = String::from("2333133121414131402");
    // let s = String::from("233313312141413140211"); // res 2132

    // remove the newline
    s.lines()
        .into_iter()
        .enumerate()
        .map(|(row, l)| line_to_topo_row(&l, row))
        .collect()
}

fn find_start_points(map: &TopoMap) -> Vec<TopoPoint> {
    let mut v = Vec::new();
    // find all the points with height 0
    for (row, r) in map.iter().enumerate() {
        for (col, tp) in r.iter().enumerate() {
            if tp.is_some() && tp.unwrap().height == 0 {
                v.push(TopoPoint {
                    p: Point {
                        x: col as i32,
                        y: row as i32,
                    },
                    height: 0,
                });
            }
        }
    }
    v
}

type MapSize = Point;
fn is_within_map(current_h: u32, m_sz: &MapSize, tp: &TopoPoint, map: &TopoMap) -> bool {
    let p = tp.p;
    if !(p.x < m_sz.x && p.y < m_sz.y && p.x >= 0 && p.y >= 0) {
        // println!("Outside map {:?}", p);
        return false;
    }

    if !map[p.y as usize][p.x as usize].is_some() {
        // println!("No pos at {:?}", p);
        // println!("{:?}", map);
        return false;
    }

    let h = map[p.y as usize][p.x as usize].unwrap().height;

    // println!("Height at {:?} is {}", p, h);
    // println!("Height {} == {} is equal: {}", h, tp.height + 1, equal);

    // check for height
    current_h + 1 == h
}

// create new TopoPoint from Point and another TopoPoint
fn create_topo_point(dir: &Point, tp: &TopoPoint) -> TopoPoint {
    TopoPoint {
        p: tp.p + *dir,
        height: tp.height + 1,
    }
}

fn get_possible_dirs(walk: &Walk, map: &TopoMap) -> Vec<TopoPoint> {
    let up = Point { x: 0, y: -1 };
    let right = Point { x: 1, y: 0 };
    let left = Point { x: -1, y: 0 };
    let down = Point { x: 0, y: 1 };
    let canonical = [
        create_topo_point(&up, &walk.pos),
        create_topo_point(&right, &walk.pos),
        create_topo_point(&left, &walk.pos),
        create_topo_point(&down, &walk.pos),
    ];

    let m_sz = Point {
        x: map.iter().next().unwrap().len() as i32,
        y: map.len() as i32,
    };
    // print possible dirs
    canonical
        .into_iter()
        .filter(|p| !walk.visited.contains(&p.p) && is_within_map(walk.pos.height, &m_sz, p, map))
        .collect()
}

fn count_walks(walk: &mut Walk, map: &TopoMap) -> () {
    // println!("Walking from {:?}", walk.pos);

    let dirs = get_possible_dirs(walk, map);

    if dirs.is_empty() {
        return;
    }

    // mark the dirs as visited
    walk.visited.extend(dirs.iter().map(|tp| tp.p));

    for dir in dirs {
        // update the walk
        // println!("Walking to {:?}", dir);
        walk.pos = dir;
        count_walks(walk, map);
    }
}

fn main() {
    let map = parse_input();
    let start_points = find_start_points(&map);

    let mut walks = create_walks(&start_points);
    // println!("{:?}", walks.len());
    // let walk = &mut walks[0];
    let mut total = 0;
    for walk in walks.iter_mut() {
        count_walks(walk, &map);
        total += walk
            .visited
            .iter()
            .filter(|p| map[p.y as usize][p.x as usize].unwrap().height == 9)
            .count();
    }
    // count the visited walks where the height is 9
    println!("{:?}", total);
}
