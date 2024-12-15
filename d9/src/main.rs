use std::collections::LinkedList;
use std::fs::File;
use std::{fs, vec};

fn parse_input() -> String {
    let s = fs::read_to_string("input.txt").expect("Should have been able to read the file");
    // let s = String::from("12345");
    // let s = String::from("2333133121414131402");
    // let s = String::from("233313312141413140211"); // res 2132

    // remove the newline
    let s: String = s.chars().filter(|c| c.is_ascii_alphanumeric()).collect();
    s
}

#[derive(Debug, Clone, Copy)]
struct DiskFile {
    id: u64,
    size: u64,
}
type Files = Vec<DiskFile>;
type Blocks = Vec<DiskFile>;

type FreeSpace = Vec<u64>;

fn expand_file(f: &DiskFile) -> Blocks {
    vec![DiskFile { id: f.id, size: 1 }].repeat(f.size as usize)
}

// needed help from copilot to figure out the lifetimes and syntax for
// reading over a mutable iterator
// take in a reversed DiskFile iterator and read n elements from it
fn read_n_from_iter<'a, I>(iter: &mut I, n: u64) -> Blocks
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
        .split_at(defragged.len() - free_space.iter().sum::<u64>() as usize)
        .0;

    println!("cs = {:?}", checksum(&compressed));
}

fn split_disk(disk: &String) -> (Files, FreeSpace) {
    let to_num = disk.chars().map(|c| c.to_digit(10).unwrap()).enumerate();
    let files: Vec<DiskFile> = to_num
        .clone()
        .filter(|(id_x2, _)| *id_x2 == 0 || id_x2 % 2 == 0)
        .map(|(id_x2, n)| DiskFile {
            id: id_x2 as u64 / 2,
            size: n as u64,
        })
        .collect();

    let mut free_space: FreeSpace = to_num
        .filter(|(id_x2, _)| id_x2 % 2 != 0)
        .map(|(_, n)| n as u64)
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

#[derive(Debug)]
struct FilesAndFreeSpace {
    files: Files,
    free: u64,
}

fn file_to_fafs(file: &DiskFile) -> FilesAndFreeSpace {
    FilesAndFreeSpace {
        files: vec![*file],
        free: 0,
    }
}

fn free_to_to_fafs(free: u64) -> FilesAndFreeSpace {
    FilesAndFreeSpace {
        files: Vec::new(),
        free,
    }
}

fn move_file(fafs: &mut Vec<FilesAndFreeSpace>, df: DiskFile) -> bool {
    let space_needed = df.size;

    let to_edit = fafs.into_iter().find(|faf| faf.free >= space_needed);
    if to_edit.is_none() {
        // no space found
        return false;
    }

    if let Some(to_edit) = to_edit {
        // update move
        to_edit.free -= space_needed;
        to_edit.files.push(df);
        return true;
    }

    return false;
}

fn del_file(fafs: &mut Vec<FilesAndFreeSpace>, df: DiskFile) -> bool {
    let to_del = fafs
        .into_iter()
        .rev()
        .find(|faf| !faf.files.is_empty() && faf.files.iter().next().unwrap().id == df.id);
    if to_del.is_none() {
        println!("Could not find file to delete");
        // no space found
        return false;
    }

    if let Some(to_del) = to_del {
        // update move
        to_del.free += df.size;
        to_del.files.pop();
        // print_disk2(fafs);
        return true;
    }

    return false;
}

fn print_disk(fafs: &Vec<FilesAndFreeSpace>) {
    for faf in fafs.iter() {
        println!("Files = {:?}, Free = {:?}", faf.files, faf.free);
    }
}

fn print_disk2(fafs: &Vec<FilesAndFreeSpace>) {
    for faf in fafs.iter() {
        for f in faf.files.iter() {
            for _ in 0..f.size {
                print!("({})", f.id);
            }
        }
        for _ in 0..faf.free {
            print!(".");
        }
    }
    println!();
}

fn defrag_disk_fast(files: &Files, free_space: &FreeSpace) -> Vec<FilesAndFreeSpace> {
    // first we convert all the FreeSpace to a new type
    // that both stores the free space and a list of files
    // every time a file is placed the free space is reduced

    // read the files backwards
    // let back_iter = files.iter.
    let mut files_and_space: Vec<FilesAndFreeSpace> = files
        .iter()
        .map(|file| file_to_fafs(file))
        .zip(free_space.iter().map(|free| free_to_to_fafs(*free)))
        .flat_map(|k| vec![k.0, k.1])
        .collect();

    let dfs_backwards: Vec<&DiskFile> = files.iter().rev().collect();
    // print_disk2(&files_and_space);
    for df in dfs_backwards.iter() {
        if move_file(&mut files_and_space, **df) {
            del_file(&mut files_and_space, **df);
        }
    }

    files_and_space
    // print_disk2(&files_and_space);
}

fn checksum2(fafs: &Vec<FilesAndFreeSpace>) -> u64 {
    let mut pos = 0;
    let mut checksum = 0;
    for faf in fafs.iter() {
        for f in faf.files.iter() {
            for _ in 0..f.size {
                checksum += pos * f.id;
                pos += 1;
            }
        }
        pos += faf.free;
    }

    checksum
}

fn part_2(files: &Files, free_space: &FreeSpace) {
    let def = defrag_disk_fast(&files, &free_space);
    println!("cs = {:?}", checksum2(&def));
}

fn main() {
    let disk = parse_input();

    let (files, free_space) = split_disk(&disk);
    // part_1(&files, &free_space);
    part_2(&files, &free_space);
}
