use std::fs;

mod day1;
mod day2;

fn read_lines(fname: &str) -> Vec<String> {
    let input = fs::read_to_string(fname)
        .expect(&format!("Unable to read input file: {}", fname));
    let mut lines = vec!();
    for line in input.split("\n") {
        if line.trim().is_empty() {
            continue;
        }
        lines.push(String::from(line));
    }
    lines
}

fn run_day(day: u32, in_file: &str, func: fn(&Vec<String>) -> (i32, i32)) {
    let input = read_lines(in_file);
    let (part1, part2) = func(&input);
    println!("Day {}", day);
    println!("  Part 1: {}", part1);
    println!("  Part 2: {}", part2);
}

fn main() {
    run_day(1, "./input/day1.txt", day1::day1);
    run_day(2, "./input/day2.txt", day2::day2);
}
