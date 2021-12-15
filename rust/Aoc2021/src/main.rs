use std::{fmt::Display, fs};

use ansi_term::Color::{Green, Red};

mod day1;
mod day2;
mod day3;
mod day4;
mod day5;
mod day6;
mod day7;

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

struct Answer {
    small: (i64, i64),
    input: (i64, i64),
}
type Handler = fn(&Vec<String>) -> (i64, i64);

fn run_day(day: u32, in_file: &str, func: Handler, answer: (i64, i64)) {
    let input = read_lines(in_file);
    let (part1, part2) = func(&input);
    println!("Day {}", day);
    println!("  {}", verify_answer("Part 1", &part1, &answer.0));
    println!("  {}", verify_answer("Part 2", &part2, &answer.1));
}

fn main() {
    let with_small = false;
    let days: Vec<u32> = if std::env::args().len() > 1 {
        std::env::args().skip(1).map(|v| v.parse().unwrap()).collect()
    } else {
        Vec::from_iter(1..=24)
    };

    let solutions: Vec<(u32, Handler, Answer)> = vec!(
        (1, day1::day1, Answer {
            small: (7, 5),
            input: (1832, 1858),
        }),
        (2, day2::day2, Answer {
            small: (150, 900),
            input: (2117664, 2073416724),
        }),
        (3, day3::day3, Answer {
            small: (198, 230),
            input: (4118544, 3832770),
        }),
        (4, day4::day4, Answer {
            small: (4512, 1924),
            input: (27027, 36975),
        }),
        (5, day5::day5, Answer {
            small: (5, 12),
            input: (8350, 19374),
        }),
        (6, day6::day6, Answer {
            small: (5934, 26984457539),
            input: (353274, 1609314870967),
        }),
        (7, day7::day7, Answer {
            small: (0, 0),
            input: (0, 0),
        }),
    );

    for (day, func, answer) in solutions {
        let input = if with_small { "small" } else { "input" };
        let answer = if with_small { answer.small } else { answer.input };
        if days.contains(&day) {
            let input = format!("./input/day{}-{}.txt", day, input);
            run_day(day, &input, func, answer);
        }
    }
}
