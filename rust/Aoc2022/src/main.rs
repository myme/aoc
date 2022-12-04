use std::{fmt::Display, fs, process::exit};

use ansi_term::Color::{Green, Red};

mod day01;
mod day02;

fn read_file(fname: &str) -> String {
    let input = fs::read_to_string(fname)
        .expect(&format!("Unable to read input file: {}", fname));
    input
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
type Handler = fn(&str) -> (i64, i64);

fn run_day(day: u32, in_file: &str, func: Handler, answer: (i64, i64)) {
    let input = read_file(in_file);
    let (part1, part2) = func(&input);
    println!("Day {:02}", day);
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
        (day01::day1, Answer {
            small: (24000, 45000),
            input: (67658, 200158),
        }),
        (day02::day2, Answer {
            small: (15, 12),
            input: (13009, 10398),
        }),
    );

    for (day, (func, answer)) in solutions.iter().enumerate() {
        let day: u32 = (day + 1).try_into().unwrap();
        if !filter_days.is_empty() && !filter_days.contains(&day) {
            continue;
        }

        let input = if small { "small" } else { "input" };
        let answer = if small { answer.small } else { answer.input };
        let input = format!("./input/day{:02}-{}.txt", day, input);

        run_day(day, &input, *func, answer);
    }
}
