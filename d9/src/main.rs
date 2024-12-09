use std::{fs, vec};

fn parse_input() -> String {
    // let s = fs::read_to_string("input.txt").expect("Should have been able to read the file");
    let s = String::from("12345");
    // let s = String::from("2333133121414131402");
    // let s = String::from("233313312141413140211"); // res 2132

    // remove the newline
    let s: String = s.chars().filter(|c| c.is_ascii_alphanumeric()).collect();
    s
}

#[derive(Debug, Clone, Copy)]
struct DiskFile {
    id: u32,
    size: u32,
}
type Files = Vec<DiskFile>;
type Blocks = Vec<DiskFile>;

type FreeSpace = Vec<u32>;

fn expand_file(f: &DiskFile) -> Blocks {
    vec![DiskFile { id: f.id, size: 1 }].repeat(f.size as usize)
}

// needed help from copilot to figure out the lifetimes and syntax for
// reading over a mutable iterator
// take in a reversed DiskFile iterator and read n elements from it
fn read_n_from_iter<'a, I>(iter: &mut I, n: u32) -> Blocks
where
    I: Iterator<Item = &'a DiskFile>,
{
    let mut res = vec![];
    for _ in 0..n {
        res.push(*iter.next().unwrap());
    }
    res
}

fn checksum(compressed: &[DiskFile]) -> usize {
    // multiply position with a
    compressed
        .iter()
        .enumerate()
        .fold(0, |acc, (i, f)| acc + i * f.id as usize)
}

fn defrag_disk(files: &Files, free_space: &FreeSpace) {
    let expanded: Blocks = files.iter().flat_map(|f| expand_file(f)).collect();

    // read all the ids backwards
    let mut to_comp = expanded.iter().rev();

    let mut defragged: Blocks = Vec::new();
    for (file, free) in files.iter().zip(free_space.iter()) {
        // first we expand the file,
        // then we fill in the free space by reading the expanded files backwards
        defragged.append(&mut expand_file(file));
        defragged.append(&mut read_n_from_iter(&mut to_comp, *free));
    }

    // our defragged disk contains duplicates, so remove those that we moved up front
    let compressed = defragged
        .split_at(defragged.len() - free_space.iter().sum::<u32>() as usize)
        .0;

    println!("cs = {:?}", checksum(&compressed));
}

fn split_disk(disk: &String) -> (Files, FreeSpace) {
    let to_num = disk.chars().map(|c| c.to_digit(10).unwrap()).enumerate();
    let files: Vec<DiskFile> = to_num
        .clone()
        .filter(|(id_x2, _)| *id_x2 == 0 || id_x2 % 2 == 0)
        .map(|(id_x2, n)| DiskFile {
            id: id_x2 as u32 / 2,
            size: n,
        })
        .collect();

    let mut free_space: FreeSpace = to_num
        .filter(|(id_x2, _)| id_x2 % 2 != 0)
        .map(|(_, n)| n as u32)
        .collect();

    // if there are more files than free space, add 0s
    if free_space.len() < files.len() {
        free_space.push(0);
    }

    (files, free_space)
}

fn part_1(files: &Files, free_space: &FreeSpace) {
    defrag_disk(&files, &free_space);
}

fn defrag_disk_fast(files: &Files, free_space: &FreeSpace) {
    // read all the ids backwards
    let mut to_comp = files.iter().rev();
    // let mut to_comp_f = files.iter();

    let mut free_space = free_space.clone();

    for file in to_comp {
        println!("{:?}", file);

        let mut first_free = free_space.iter().find(|fs| *fs >= &file.size);

        if let Some(x) = first_free {
            println!("{:?}", x);
        }
    }

    // algo:
    // 1. scan the files _backwards_
    // 2. for each file, find the first contiguous place where we can place the file
    // 3. we keep track of all the used space
    // let mut defragged: Blocks = Vec::new();
    // for (file, free) in files.iter().zip(free_space.iter()) {
    //     // first we expand the file,
    //     // then we fill in the free space by reading the expanded files backwards
    //     defragged.append(&mut expand_file(file));
    //     defragged.append(&mut read_n_from_iter(&mut to_comp, *free));
    // }

    // // our defragged disk contains duplicates, so remove those that we moved up front
    // let compressed = defragged
    //     .split_at(defragged.len() - free_space.iter().sum::<u32>() as usize)
    //     .0;

    // println!("cs = {:?}", checksum(&compressed));
}

fn part_2(files: &Files, free_space: &FreeSpace) {
    defrag_disk_fast(&files, &free_space);
}

fn main() {
    let disk = parse_input();

    let (files, free_space) = split_disk(&disk);
    // part_1(&files, &free_space);
    part_2(&files, &free_space);
}
