use crate::utils;

fn part1(_numbers: &Vec<i32>) -> i32 {
    0
}

fn part2(_numbers: &Vec<i32>) -> i32 {
    0
}

pub fn day1(input: &str) -> (i64, i64) {
    let mut numbers: Vec<i32> = vec!();
    for line in utils::to_lines(input) {
        numbers.push(line.parse().unwrap());
    }

    (part1(&numbers).into(), part2(&numbers).into())
}
