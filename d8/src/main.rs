use itertools::Itertools;
use std::collections::HashMap;
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

type AntennaLocs = Vec<Point>;
type ResonantPoints = Vec<Point>;
type Antennas = HashMap<char, Vec<Point>>;
type MapSize = Point;

fn parse_input() -> (Antennas, MapSize) {
    let s = fs::read_to_string("input.txt").expect("Should have been able to read the file");
    let _s = String::from(
        r"............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............",
    );

    let lines = s.split_terminator('\n');
    let rows = lines.clone().into_iter().count();
    let cols = lines.clone().into_iter().next().unwrap().len();

    let valid_lines = lines
        .into_iter()
        .enumerate()
        .filter(|(_, line)| line.contains(|c: char| c.is_ascii_alphanumeric()));

    let mut antennas: Antennas = HashMap::new();

    // TODO: this for loop can just be incorporated ito the above chain
    for (row, line) in valid_lines {
        let ind = line.char_indices();
        let valid_antennas = ind.into_iter().filter(|(_, c)| *c != '.');

        // now we iterate over these antennas and insert each into our hash map
        for (col, c_ant) in valid_antennas {
            if !antennas.contains_key(&c_ant) {
                antennas.insert(c_ant, Vec::new());
            }
            if let Some(p) = antennas.get_mut(&c_ant) {
                p.push(Point {
                    x: col as i32,
                    y: row as i32,
                });
            }
        }
    }

    (
        antennas,
        MapSize {
            x: cols as i32,
            y: rows as i32,
        },
    )
}

fn find_resonance_point(loc1: &Point, loc2: &Point) -> Point {
    /*
    ............
    ............
    ............
    .......0....  <- loc 1
    ....0.......  <- loc 2
    .x..........  <- generates this resonance
    ............
    ............
    ............
    ............
    ............
    ............
    */

    // above, the diff would be {x = -3, y = 1}
    // now, this difference should be added to loc2 to give

    // println!("F")
    let diff_vec = *loc2 - *loc1;

    // println!(
    //     "loc1 {:?} and loc2 {:?} generates {:?}",
    //     loc1,
    //     loc2,
    //     *loc2 + diff_vec
    // );
    *loc2 + diff_vec
}

// given a list of antenna locations, where do the resonant frequencies fall?
fn calc_resonance_points(locs: &AntennaLocs) -> ResonantPoints {
    locs.iter()
        .flat_map(|loc1| {
            locs.iter()
                .filter(|loc2| *loc1 != **loc2)
                .map(|loc2| find_resonance_point(loc1, loc2))
        })
        .collect()
}

fn is_within_map(m_sz: &MapSize, p: &Point) -> bool {
    p.x < m_sz.x && p.y < m_sz.y && p.x >= 0 && p.y >= 0
}

fn main() {
    let (antennas, m_sz) = parse_input();
    // println!("{:?}", antennas);
    // let first_entry = antennas.get(&'0');
    // println!("{:?}", calc_resonance_points(&first_entry.unwrap()));

    let mut total: Vec<Point> = antennas
        .iter()
        .flat_map(|(_, locs)| calc_resonance_points(locs))
        .filter(|p| is_within_map(&m_sz, p))
        .collect();

    total.sort();
    total.dedup(); // we need to find unique locations

    // println!("Map size = {:?}", m_sz);

    // for rloc in total {
    //     println!("{:?}", rloc);
    // }

    println!("p1 = {:?}", total.len());
}
