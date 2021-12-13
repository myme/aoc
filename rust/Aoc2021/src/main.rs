use std::{fmt::Display, fs};

use ansi_term::Color::{Green, Red};

mod day1;
mod day2;
mod day3;
mod day4;

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

fn verify_answer<T: PartialEq + Display>(label: &str, actual: &T, expected: &T) -> String {
    if actual == expected {
        format!("{} {}: {}", Green.paint("✔"), label, actual)
    } else {
        format!("{} {}: {} (expected: {})", Red.paint("✘"), label, actual, expected)
    }
}

fn run_day(
    day: u32,
    in_file: &str,
    func: fn(&Vec<String>) -> (i32, i32),
    answer: (i32, i32)
) {
    let input = read_lines(in_file);
    let (part1, part2) = func(&input);
    println!("Day {}", day);
    println!("  {}", verify_answer("Part 1", &part1, &answer.0));
    println!("  {}", verify_answer("Part 2", &part2, &answer.1));
}

fn main() {
    run_day(1, "./input/day1-input.txt", day1::day1, (1832, 1858));
    run_day(2, "./input/day2-input.txt", day2::day2, (2117664, 2073416724));
    run_day(3, "./input/day3-input.txt", day3::day3, (4118544, 3832770));
    run_day(4, "./input/day4-input.txt", day4::day4, (27027, 36975));
}
