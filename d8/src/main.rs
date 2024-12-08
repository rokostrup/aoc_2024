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

    let lines = s.split_terminator('\n');
    let rows = lines.clone().into_iter().count();
    let cols = lines.clone().into_iter().next().unwrap().len();

    let valid_lines = lines
        .into_iter()
        .enumerate()
        .filter(|(_, line)| line.contains(|c: char| c.is_ascii_alphanumeric()));

    let mut antennas: Antennas = HashMap::new();

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

fn find_resonance_points(loc1: &Point, loc2: &Point, repl: bool, m_sz: &MapSize) -> ResonantPoints {
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

    let diff_vec = *loc2 - *loc1;
    let mut res_loc = *loc2 + diff_vec;
    if !repl {
        return vec![res_loc];
    }

    // for two antennas we always hit the antenna locs
    let mut all_res: ResonantPoints = vec![*loc1, *loc2];
    while is_within_map(m_sz, &res_loc) {
        all_res.push(res_loc);
        // go to next resonance point
        res_loc = res_loc + diff_vec;
    }

    all_res
}

// given a list of antenna locations, where do the resonant frequencies fall?
fn calc_resonance_points(locs: &AntennaLocs, repl: bool, m_sz: &MapSize) -> ResonantPoints {
    locs.iter()
        .flat_map(|loc1| {
            locs.iter()
                .filter(|loc2| *loc1 != **loc2)
                .flat_map(|loc2| find_resonance_points(loc1, loc2, repl, m_sz))
        })
        .collect()
}

fn is_within_map(m_sz: &MapSize, p: &Point) -> bool {
    p.x < m_sz.x && p.y < m_sz.y && p.x >= 0 && p.y >= 0
}

fn solve(antennas: &Antennas, repl: bool, m_sz: &MapSize) -> () {
    let mut total: Vec<Point> = antennas
        .iter()
        .flat_map(|(_, locs)| calc_resonance_points(locs, repl, &m_sz))
        .filter(|p| is_within_map(&m_sz, p))
        .collect();

    total.sort();
    total.dedup(); // we need to find unique locations

    println!("{:?}", total.len());
}

fn main() {
    let (antennas, m_sz) = parse_input();

    let part_1_repl = false;
    solve(&antennas, part_1_repl, &m_sz);
    let part_2_repl = true;
    solve(&antennas, part_2_repl, &m_sz);
}
