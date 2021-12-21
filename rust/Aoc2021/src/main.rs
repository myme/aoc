use std::{fmt::Display, fs, process::exit};

use ansi_term::Color::{Green, Red};

mod day1;
mod day2;
mod day3;
mod day4;
mod day5;
mod day6;
mod day7;
mod day8;
mod day9;
mod day10;

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

struct Args {
    help: bool,
    small: bool,
    filter_days: Vec<u32>,
}

fn parse_args() -> Args {
    let mut args = Args {
        help: false,
        small: false,
        filter_days: vec![],
    };

    for arg in std::env::args().skip(1) {
        if arg == "-h" || arg == "--help" {
            args.help = true;
        } else if arg == "-s" || arg == "--small" {
            args.small = true;
        } else {
            match arg.parse::<u32>() {
                Ok(d) => args.filter_days.push(d),
                Err(_) => {
                    usage();
                    println!("Invalid argument: {}", arg);
                    exit(1);
                },
            }
        }
    }

    args
}

fn usage() {
    println!("usage: [-s|--small] [days...]");
}

fn main() {
    let Args { help, small, filter_days } = parse_args();

    if help {
        usage();
        exit(0);
    }

    let solutions: Vec<(Handler, Answer)> = vec!(
        (day1::day1, Answer {
            small: (7, 5),
            input: (1832, 1858),
        }),
        (day2::day2, Answer {
            small: (150, 900),
            input: (2117664, 2073416724),
        }),
        (day3::day3, Answer {
            small: (198, 230),
            input: (4118544, 3832770),
        }),
        (day4::day4, Answer {
            small: (4512, 1924),
            input: (27027, 36975),
        }),
        (day5::day5, Answer {
            small: (5, 12),
            input: (8350, 19374),
        }),
        (day6::day6, Answer {
            small: (5934, 26984457539),
            input: (353274, 1609314870967),
        }),
        (day7::day7, Answer {
            small: (37, 168),
            input: (344735, 96798233),
        }),
        (day8::day8, Answer {
            small: (26, 61229),
            input: (344, 1048410),
        }),
        (day9::day9, Answer {
            small: (15, 1134),
            input: (491, 1075536),
        }),
        (day10::day10, Answer {
            small: (0, 0),
            input: (0, 0),
        }),
    );

    for (day, (func, answer)) in solutions.iter().enumerate() {
        let day: u32 = (day + 1).try_into().unwrap();
        if !filter_days.is_empty() && !filter_days.contains(&day) {
            continue;
        }

        let input = if small { "small" } else { "input" };
        let answer = if small { answer.small } else { answer.input };
        let input = format!("./input/day{}-{}.txt", day, input);

        run_day(day, &input, *func, answer);
    }
}
