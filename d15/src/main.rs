use std::fs;

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

enum Tile {
    Box,
    Empty,
    Wall,
    Robot,
}

struct Location {
    pos: Point,
    tile: Tile,
}

type Row = Vec<Location>;
type Map = Vec<Row>;

#[derive(Debug)]
enum Move {
    North,
    South,
    East,
    West,
}

type Moves = Vec<Move>;

fn move_to_dir(m: Move) -> Point {
    match m {
        Move::North => Point { x: 0, y: -1 },
        Move::South => Point { x: 0, y: 1 },
        Move::East => Point { x: 1, y: 0 },
        Move::West => Point { x: -1, y: 0 },
    }
}

fn c_to_tile(c: char) -> Tile {
    match c {
        '.' => Tile::Empty,
        '#' => Tile::Wall,
        'O' => Tile::Box,
        '@' => Tile::Robot,
        _ => panic!("Invalid tile character"),
    }
}

fn parse_row(rstr: &str, row: i64) -> Row {
    rstr.char_indices()
        .map(|(col, c)| Location {
            pos: Point {
                x: col as i64,
                y: row,
            },
            tile: c_to_tile(c),
        })
        .collect()
}

fn parse_map(mstr: &str) -> Map {
    mstr.lines()
        .enumerate()
        .map(|(row, rstr)| parse_row(rstr, row as i64))
        .collect()
}

fn parse_movement(mstr: &str) -> Moves {
    mstr.chars()
        .filter(|c| *c != '\n')
        .map(|c| match c {
            '^' => Move::North,
            'v' => Move::South,
            '>' => Move::East,
            '<' => Move::West,
            _ => panic!("Invalid movement character"),
        })
        .collect()
}

fn parse_input() -> (Map, Moves) {
    let _s = String::from(
        r"########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<",
    );

    let _s = String::from(
        r"##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^",
    );

    let s = fs::read_to_string("input.txt").unwrap();

    let mut split = s.split_terminator("\n\n");

    (
        parse_map(split.next().unwrap()),
        parse_movement(split.next().unwrap()),
    )
}

fn is_within_map(map: &Map, pos: Point) -> bool {
    pos.x >= 0 && pos.y >= 0 && pos.x < map[0].len() as i64 && pos.y < map.len() as i64
}

fn find_first_empty_space_from_pos(map: &Map, pos: &Point, dir: &Point) -> Option<Point> {
    // given a position and a move (direction) find the first empty space in that direction
    // before hitting a wall
    let mut new_pos = *pos + *dir;
    while is_within_map(map, new_pos) {
        // println!("Searching {:?} in dir {:?}", new_pos, dir);

        match &map[new_pos.y as usize][new_pos.x as usize].tile {
            Tile::Empty => {
                // println!("Found empty space at {:?}", new_pos);
                return Some(new_pos);
            }
            Tile::Wall => {
                // println!("Hit wall at {:?}", new_pos);
                return None;
            }
            _ => (),
        }
        new_pos += *dir;
    }
    None
}

fn get_location(map: &Map, pos: Point) -> &Location {
    &map[pos.y as usize][pos.x as usize]
}

// will also update map with box move
fn try_push_box(new_pos: &Point, dir: &Point, map: &mut Map) -> bool {
    // try find first empty space in the direction of the box
    let new_box_pos = find_first_empty_space_from_pos(map, new_pos, dir);

    match new_box_pos {
        Some(p) => {
            // move the box to the new position
            map[new_pos.y as usize][new_pos.x as usize].tile = Tile::Empty;
            map[p.y as usize][p.x as usize].tile = Tile::Box;
            true
        }
        None => false,
    }
}

fn update_move_on_map(pos: Point, new_pos: Point, map: &mut Map) -> Point {
    map[pos.y as usize][pos.x as usize].tile = Tile::Empty;
    map[new_pos.y as usize][new_pos.x as usize].tile = Tile::Robot;
    new_pos
}

fn update_move(pos: Point, m: Move, map: &mut Map) -> Point {
    let dir = move_to_dir(m);
    let new_pos = pos + dir;
    // println!("Moving from {:?} to {:?} in dir = {:?}", pos, new_pos, dir);

    let loc = get_location(&map, new_pos);

    match loc.tile {
        Tile::Empty => update_move_on_map(pos, new_pos, map),
        Tile::Wall => pos, // hit a wall, do nothing
        Tile::Box => {
            if try_push_box(&new_pos, &dir, map) {
                update_move_on_map(pos, new_pos, map)
            } else {
                pos // could not move the box, do nothing
            }
        }
        _ => panic!("Invalid tile"),
    }
}

fn find_robot(map: &Map) -> Point {
    map.iter()
        .find(|r| {
            r.iter().any(|l| match l.tile {
                Tile::Robot => true,
                _ => false,
            })
        })
        .unwrap()
        .iter()
        .find(|l| match l.tile {
            Tile::Robot => true,
            _ => false,
        })
        .unwrap()
        .pos
}

// print map
fn print_map(map: &Map) {
    for row in map {
        for loc in row {
            match loc.tile {
                Tile::Empty => print!("."),
                Tile::Wall => print!("#"),
                Tile::Box => print!("O"),
                Tile::Robot => print!("@"),
            }
        }
        println!();
    }
}

fn calc_gps_pos(pos: &Point) -> usize {
    (pos.y * 100 + pos.x) as usize
}

fn calc_all_gps(map: &Map) -> usize {
    let mut gps: usize = 0;
    for r in map.iter() {
        for l in r.iter() {
            match l.tile {
                Tile::Box => {
                    gps += calc_gps_pos(&l.pos);
                }
                _ => (),
            }
        }
    }
    gps
    // map.iter()
    //     .filter(|r| {
    //         r.iter().any(|l| match l.tile {
    //             Tile::Box => true,
    //             _ => false,
    //         })
    //     })
    //     .flat_map(|r| r.iter().map(|l| calc_gps_pos(l)))
    //     .sum()
}

fn main() {
    let (mut map, moves) = parse_input();
    let mut pos = find_robot(&map);
    for m in moves {
        pos = update_move(pos, m, &mut map);
        // print_map(&map);
    }

    println!("Final score: {:?}", calc_all_gps(&map));
}
