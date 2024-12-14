use std::collections::{HashMap, HashSet};
use std::fs;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
struct Point {
    x: i32,
    y: i32,
}

#[derive(Debug)]
struct Vegetable {
    p: Point,
    v: char,
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

type GardenRow = Vec<Vegetable>;
type Garden = Vec<GardenRow>;
type Plot = Vec<Point>;
type Plots = Vec<Plot>;

fn line_to_row(l: &str, row: i32) -> GardenRow {
    l.char_indices()
        .map(|(col, v)| Vegetable {
            p: Point {
                x: col as i32,
                y: row,
            },
            v,
        })
        .collect()
}

fn parse_input() -> Garden {
    let _s = String::from(
        r"AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA",
    );

    let _s = String::from(
        r"OOOOO
OXOXO
OOOOO
OXOXO
OOOOO",
    );

    let _s = String::from(
        r"AAAA
BBCD
BBCC
EEEC",
    );

    let s = String::from(
        r"RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE",
    );

    let s = fs::read_to_string("input.txt").expect("Should have been able to read the file");

    s.lines()
        .enumerate()
        .map(|(row, l)| line_to_row(l, row as i32))
        .collect()
}

fn get_vegetable_types(garden: &Garden) -> Vec<char> {
    let mut types: Vec<char> = garden
        .iter()
        .flat_map(|v| v.into_iter().map(|v| v.v))
        .collect();

    types.sort();
    types.dedup();
    types
}

type MapSize = Point;
fn is_within_map(p: &Point, m_sz: &MapSize) -> bool {
    p.x < m_sz.x && p.y < m_sz.y && p.x >= 0 && p.y >= 0
}

// must be valid point
fn get_vegetable(garden: &Garden, p: &Point) -> char {
    garden[p.y as usize][p.x as usize].v
}

fn get_possible_neigbors(p: &Point) -> Vec<Point> {
    vec![
        Point { x: p.x, y: p.y - 1 },
        Point { x: p.x, y: p.y + 1 },
        Point { x: p.x - 1, y: p.y },
        Point { x: p.x + 1, y: p.y },
    ]
}

fn get_valid_neighbours(v: &Vegetable, garden: &Garden) -> Vec<Vegetable> {
    // map size
    let m_sz = Point {
        x: garden[0].len() as i32,
        y: garden.len() as i32,
    };
    // find north west east and south of v
    let neighbours = get_possible_neigbors(&v.p);

    neighbours
        .iter()
        .filter(|p| is_within_map(*p, &m_sz) && get_vegetable(&garden, p) == v.v)
        .map(|p| Vegetable { p: *p, v: v.v })
        .collect()
}

fn get_invalid_neighbours(v: &Vegetable, garden: &Garden) -> Vec<Point> {
    // map size
    let m_sz = Point {
        x: garden[0].len() as i32,
        y: garden.len() as i32,
    };
    // find north west east and south of v
    let neighbours = get_possible_neigbors(&v.p);

    neighbours
        .into_iter()
        .filter(|p| !is_within_map(p, &m_sz) || get_vegetable(&garden, p) != v.v)
        .collect()
}

// find the given plot of vegetable (can be extracted from visited)
fn find_plot(v: &Vegetable, garden: &Garden, visited: &mut HashSet<Point>) {
    if !visited.insert(v.p) {
        // println!("Already visited {:?}", v.p);
        return; // already been here
    }

    let neighbours = get_valid_neighbours(v, garden);

    // println!("Valid Neighbours of {:?} are {:?}", v, neighbours);

    neighbours
        .iter()
        .for_each(|v| find_plot(v, garden, visited));
}

fn find_veggie_plots(v: char, garden: &Garden) -> Plots {
    // we scan the garden for the vegetable.
    // whenver once is accounted, we search recursively
    // for the same type that is neigbourging
    // and add that to the collection of plots

    let mut visited: HashSet<Point> = HashSet::new();

    let mut plots = Vec::new();

    // for row in garden
    for row in garden {
        for veg in row {
            if veg.v != v || visited.contains(&veg.p) {
                continue;
            }

            let mut local_visited: HashSet<Point> = HashSet::new();
            find_plot(veg, garden, &mut local_visited);

            // add to plot to plots
            // println!("Plots len = {:?}", plots.len());
            plots.push(local_visited.clone().into_iter().collect::<Vec<Point>>());
            // println!("Plots len = {:?}", plots);
            // cache result for speed up
            visited.extend(local_visited.into_iter());
            // println!("Visited = {:?}", visited);
        }
    }

    plots
}

fn calc_number_of_neigbors(p: &Point, plot: &Plot) -> usize {
    let neighbours = get_possible_neigbors(&p);

    neighbours.iter().filter(|pp| plot.contains(pp)).count()
}

fn calc_perimeter(plot: &Plot) -> usize {
    // for each point in the plot that does NOT have any neighbour,
    // we need a perimeter. this can come in one of 3 variants:
    // 1 neighbor  => 3 fences
    // 2 neighbors => 2 fences
    // 3 neighbors => 1 fence
    // 4 neighbors => 0 fence
    // in general, the number of fences is 4 - number of neigbors

    plot.iter()
        .map(|p| 4 - calc_number_of_neigbors(p, plot))
        .sum()
}

fn calc_plot_cost(plot: &Plot) -> usize {
    calc_perimeter(plot) * plot.len()
}

fn part_1() {
    let garden = parse_input();

    let veggie_types = get_vegetable_types(&garden);
    let all_plots: Plots = veggie_types
        .iter()
        .flat_map(|v| find_veggie_plots(*v, &garden))
        .collect();
    // for row in garden {
    // println!("{:?}", get_vegetable_types(&garden));

    // let plots = find_veggie_plots('B', &garden);
    // println!("Found plots = {:?}", plots);

    let total: usize = all_plots.iter().map(|plot| calc_plot_cost(plot)).sum();
    println!("Total cost  = {:?}", total);

    // let mut visited = HashSet::new();
    // let veg = Vegetable {
    //     p: Point { x: 2, y: 1 },
    //     v: 'C',
    // };
    // find_plot(&veg, &garden, &mut visited);
    // println!("C = {:?}", visited);
}

type FenceCount = HashMap<Point, u32>;

fn create_fence_for_plot(plot: &Plot) -> FenceCount {
    // for each point, find all possible neighbours.
    // a possible neighbor is someone who is not already on the plot

    let fences: Vec<Point> = plot
        .iter()
        .map(|p| get_possible_neigbors(p))
        .flat_map(|pnv| pnv.into_iter().filter(|pn| !plot.contains(pn)))
        .collect();

    let mut multi_fence = HashMap::new();

    // organize the fences nicely
    for fp in fences {
        // if multi_fence.
        if let Some(mf) = multi_fence.get_mut(&fp) {
            *mf += 1;
        } else {
            multi_fence.insert(fp, 1);
        }
    }

    multi_fence
}

fn walk_in_dir(fc: &FenceCount, fp: &Point, dir: Point) -> Vec<Point> {
    // we start at a point and keep going in a direction
    let mut next_point = *fp + dir;

    let mut vp = vec![*fp];

    while fc.contains_key(&next_point) {
        vp.push(next_point);
        next_point = next_point + dir;
    }

    vp
}

// given a point, fp, construct 2 fences (East-West and North-South)
// then return the longest
fn construct_longest_fence(fc: &FenceCount, fp: &Point) -> Vec<Point> {
    // we start at a point and keep going in a direction

    let north = Point { x: 0, y: -1 };
    let south = Point { x: 0, y: 1 };
    let east = Point { x: 1, y: 0 };
    let west = Point { x: -1, y: 0 };

    let mut ew_fence = walk_in_dir(fc, fp, east);
    ew_fence.extend(walk_in_dir(fc, fp, west));
    ew_fence.sort();
    ew_fence.dedup();

    let mut ns_fence = walk_in_dir(fc, fp, north);
    ns_fence.extend(walk_in_dir(fc, fp, south));
    ns_fence.sort();
    ns_fence.dedup();

    if ew_fence.len() > ns_fence.len() {
        ew_fence
    } else {
        ns_fence
    }
}

fn find_next_unique_fence(fc: &FenceCount) -> Vec<Point> {
    let fp = fc.iter().next().unwrap().0;
    construct_longest_fence(fc, fp)
}

fn find_unique_fences(fc: &mut FenceCount) -> usize {
    let mut fence_count = 0;
    while !fc.is_empty() {
        fence_count += 1;
        let unique_fence = find_next_unique_fence(fc);

        // now we subtract all the Points we just used for the fence
        for p in unique_fence {
            let fp = fc.get_mut(&p).unwrap();
            *fp -= 1;

            // clean up
            if *fp == 0 {
                fc.remove(&p);
            }
        }
    }
    fence_count
}

fn part_2() {
    let garden = parse_input();

    let veggie_types = get_vegetable_types(&garden);
    let all_plots: Plots = veggie_types
        .iter()
        .flat_map(|v| find_veggie_plots(*v, &garden))
        .collect();

    let mut total = 0;
    let total_plots = all_plots.len();
    for plot in all_plots {
        let mut fences = create_fence_for_plot(&plot);

        if plot.len() == 101 {
            // println!("Plot = {:?}", plot);
            println!("Fences LIST = {:?}", fences);
            println!("Fences IN = {:?}", fences.len());
        }

        let fences_count = find_unique_fences(&mut fences);
        let plot_price = fences_count * plot.len();

        if plot.len() == 101 {
            println!("Fences OUT = {:?}", fences_count);
        }

        // println!(
        //     "MY plots {} * price {} = {}",
        //     plot.len(),
        //     fences_count,
        //     plot_price
        // );

        total += plot_price;
        // println!("Found {}", fences_count * plot.len());
    }
    println!("Plots found = {}", total_plots);
    println!("Total {}", total);
    // println!("{:?} {}", fences, fences.len());
}

fn debugfn() {
    let mut fences: FenceCount = HashMap::new();
    fences.insert(Point { x: 52, y: 21 }, 1);
    fences.insert(Point { x: 60, y: 23 }, 1);
    fences.insert(Point { x: 61, y: 22 }, 2);
    fences.insert(Point { x: 58, y: 23 }, 1);
    fences.insert(Point { x: 52, y: 14 }, 1);
    fences.insert(Point { x: 57, y: 22 }, 1);
    fences.insert(Point { x: 58, y: 8 }, 1);
    fences.insert(Point { x: 55, y: 23 }, 2);
    fences.insert(Point { x: 54, y: 16 }, 2);
    fences.insert(Point { x: 61, y: 20 }, 3);
    fences.insert(Point { x: 53, y: 15 }, 2);
    fences.insert(Point { x: 61, y: 17 }, 2);
    fences.insert(Point { x: 62, y: 11 }, 1);
    fences.insert(Point { x: 62, y: 12 }, 1);
    fences.insert(Point { x: 62, y: 13 }, 1);
    fences.insert(Point { x: 52, y: 13 }, 1);
    fences.insert(Point { x: 51, y: 19 }, 1);
    fences.insert(Point { x: 55, y: 9 }, 1);
    fences.insert(Point { x: 51, y: 20 }, 1);
    fences.insert(Point { x: 61, y: 14 }, 3);
    fences.insert(Point { x: 61, y: 10 }, 1);
    fences.insert(Point { x: 62, y: 21 }, 1);
    fences.insert(Point { x: 52, y: 12 }, 1);
    fences.insert(Point { x: 59, y: 23 }, 1);
    fences.insert(Point { x: 62, y: 19 }, 1);
    fences.insert(Point { x: 54, y: 11 }, 3);
    fences.insert(Point { x: 53, y: 22 }, 1);
    fences.insert(Point { x: 56, y: 9 }, 2);
    fences.insert(Point { x: 52, y: 11 }, 1);
    fences.insert(Point { x: 57, y: 8 }, 1);
    fences.insert(Point { x: 53, y: 10 }, 1);
    fences.insert(Point { x: 56, y: 22 }, 2);
    fences.insert(Point { x: 61, y: 18 }, 2);
    fences.insert(Point { x: 52, y: 17 }, 1);
    fences.insert(Point { x: 51, y: 18 }, 1);
    fences.insert(Point { x: 54, y: 10 }, 1);
    fences.insert(Point { x: 53, y: 21 }, 2);
    fences.insert(Point { x: 57, y: 21 }, 3);
    fences.insert(Point { x: 53, y: 17 }, 1);
    fences.insert(Point { x: 59, y: 9 }, 1);
    fences.insert(Point { x: 54, y: 17 }, 2);
    fences.insert(Point { x: 62, y: 16 }, 1);
    fences.insert(Point { x: 59, y: 11 }, 3);
    fences.insert(Point { x: 60, y: 10 }, 1);
    fences.insert(Point { x: 59, y: 10 }, 1);
    fences.insert(Point { x: 62, y: 15 }, 1);
    fences.insert(Point { x: 54, y: 24 }, 1);
    fences.insert(Point { x: 53, y: 23 }, 1);

    let fences_count = find_unique_fences(&mut fences);

    println!("COUNT = {}", fences_count);
}

fn main() {
    // part_2();
    debugfn();
}
