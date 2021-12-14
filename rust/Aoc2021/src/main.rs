use std::{fmt::Display, fs};

use ansi_term::Color::{Green, Red};

mod day1;
mod day2;
mod day3;
mod day4;
mod day5;
mod day6;

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

type Handler = fn(&Vec<String>) -> (i32, i32);

fn main() {
    let days: Vec<u32> = if std::env::args().len() > 1 {
        std::env::args().skip(1).map(|v| v.parse().unwrap()).collect()
    } else {
        Vec::from_iter(1..=24)
    };

    let solutions: Vec<(u32, &str, Handler, (i32, i32))> = vec!(
        (1, "./input/day1-input.txt", day1::day1, (1832, 1858)),
        (2, "./input/day2-input.txt", day2::day2, (2117664, 2073416724)),
        (3, "./input/day3-input.txt", day3::day3, (4118544, 3832770)),
        (4, "./input/day4-input.txt", day4::day4, (27027, 36975)),
        (5, "./input/day5-input.txt", day5::day5, (8350, 19374)),
        (6, "./input/day6-input.txt", day6::day6, (353274, 0)),
    );

    for (day, input, func, answer) in solutions {
        if days.contains(&day) {
            run_day(day, input, func, answer);
        }
    }
}
