use std::fs;

fn get_contents() -> String {
    fs::read_to_string("input.txt").expect("Should have been able to read the file")

    //     String::from(
    //         r"MMMSXXMASM
    // MSAMXMSMSA
    // AMXSXMAAMM
    // MSAMASMSMX
    // XMASAMXAMM
    // XXAMMXXAMA
    // SMSMSASXSS
    // SAXAMASAAA
    // MAMMMXMMMM
    // MXMXAXMASX",
    //     )
}

struct Point {
    x: usize,
    y: usize,
}

struct CharPoint {
    p: Point,
    c: u8,
}

type CharLine = Vec<CharPoint>;

fn create_2d_view(s: &String) -> Vec<&str> {
    s.split('\n').collect()
}

// check for a single match
fn matches(char_vec: &CharLine, grid: &Vec<&str>, offset: Point) -> bool {
    // check that all points matches the location
    for cp in char_vec {
        let p = &cp.p;
        let y = p.y + offset.y;
        if y >= grid.len() {
            return false;
        }

        let row = grid[y];
        let x = p.x + offset.x;
        if x >= row.len() {
            return false;
        }

        if row.as_bytes()[x] != cp.c {
            return false;
        }
    }
    return true;
}

fn cnt_matches(char_vec: &CharLine, grid: &Vec<&str>) -> usize {
    if grid.is_empty() {
        return 0;
    }

    let cols = grid[0].len();
    let mut cnt = 0;
    for y in 0..grid.len() {
        for x in 0..cols {
            if matches(char_vec, grid, Point { x, y }) {
                cnt += 1;
            }
        }
    }

    cnt
}

fn create_needle(s: &String, x_step: usize, y_step: usize, start: &Point) -> CharLine {
    let mut cl = Vec::new();

    let mut x = start.x;
    let mut y = start.y;
    for byte in s.as_bytes() {
        cl.push(CharPoint {
            p: Point { x, y },
            c: *byte,
        });
        x += x_step;
        if y_step < 1000 {
            y += y_step;
        } else if y > 0 {
            y -= 1;
        }
    }

    cl
}

fn create_needle_vec() -> Vec<CharLine> {
    let s = String::from("XMAS");
    let s_rev = s.chars().rev().collect();
    let mut needles = Vec::new();

    let origo = Point { x: 0, y: 0 };
    needles.push(create_needle(&s, 1, 0, &origo));
    needles.push(create_needle(&s, 1, 1, &origo));
    needles.push(create_needle(&s, 0, 1, &origo));
    needles.push(create_needle(&s_rev, 1, 0, &origo));
    needles.push(create_needle(&s_rev, 1, 1, &origo));
    needles.push(create_needle(&s_rev, 0, 1, &origo));
    let offset = Point { x: 0, y: 3 };
    needles.push(create_needle(&s, 1, 10000, &offset));
    needles.push(create_needle(&s_rev, 1, 10000, &offset));

    needles
}

fn main() {
    let c = get_contents();
    let grid = create_2d_view(&c);
    let needles = create_needle_vec();
    let mut sum = 0;

    for n in needles {
        sum += cnt_matches(&n, &grid);
    }
    println!("{}", sum);
}
