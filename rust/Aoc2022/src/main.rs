use std::{fs, process::exit};

use ansi_term::Color::{Green, Red};

mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;

fn read_file(fname: &str) -> String {
    let input = fs::read_to_string(fname)
        .expect(&format!("Unable to read input file: {}", fname));
    input
}

fn verify_answer(label: &str, actual: &str, expected: &str) -> String {
    if actual == expected {
        format!("{} {}: {}", Green.paint("✔"), label, actual)
    } else {
        format!("{} {}: {} (expected: {})", Red.paint("✘"), label, actual, expected)
    }
}

struct Answer<'a> {
    small: (&'a str, &'a str),
    input: (&'a str, &'a str),
}
type Handler = fn(&str) -> (String, String);

fn run_day(day: u32, in_file: &str, func: Handler, answer: (&str, &str)) {
    let input = read_file(in_file);
    let (part1, part2) = func(&input);
    println!("Day {:02}", day);
    println!("  {}", verify_answer("Part 1", &part1, answer.0));
    println!("  {}", verify_answer("Part 2", &part2, answer.1));
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
            small: ("24000", "45000"),
            input: ("67658", "200158"),
        }),
        (day02::day2, Answer {
            small: ("15", "12"),
            input: ("13009", "10398"),
        }),
        (day03::day3, Answer {
            small: ("157", "70"),
            input: ("8105", "2363"),
        }),
        (day04::day4, Answer {
            small: ("2", "4"),
            input: ("576", "905"),
        }),
        (day05::day5, Answer {
            small: ("CMZ", "MCD"),
            input: ("CVCWCRTVQ", "CNSCZWLVT"),
        }),
        (day06::day6, Answer {
            small: ("7", "0"),
            input: ("1647", "0"),
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
