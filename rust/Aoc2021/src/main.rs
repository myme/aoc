use std::fs;

mod day1;
mod day2;
mod day3;

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

fn run_day(
    day: u32,
    in_file: &str,
    func: fn(&Vec<String>) -> (i32, i32),
    answer: (i32, i32)
) {
    let input = read_lines(in_file);
    let (part1, part2) = func(&input);
    assert_eq!(part1, answer.0);
    assert_eq!(part2, answer.1);
    println!("Day {}", day);
    println!("  Part 1: {}", part1);
    println!("  Part 2: {}", part2);
}

fn main() {
    run_day(1, "./input/day1.txt", day1::day1, (1832, 1858));
    run_day(2, "./input/day2.txt", day2::day2, (2117664, 2073416724));
    run_day(3, "./input/day3.txt", day3::day3, (4118544, 3832770));
}
