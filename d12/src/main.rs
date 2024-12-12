use std::collections::HashSet;
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

    let _s = String::from(
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

fn main() {
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
