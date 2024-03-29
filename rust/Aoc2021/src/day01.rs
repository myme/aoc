use std::collections::VecDeque;

use crate::utils;

fn part1(numbers: &Vec<i32>) -> u64 {
    let mut larger = 0u64;
    let mut prev: Option<i32> = None;

    for number in numbers {
        if let Some(p) = prev {
            if number > &p {
                larger += 1;
            }
        }
        prev = Some(*number);
    }

    larger
}

fn part2(numbers: &Vec<i32>) -> u64 {
    let mut larger = 0u64;
    let mut deque = VecDeque::new();

    for number in numbers {
        deque.push_back(*number);
        if deque.len() == 4 {
            let first = deque[0] + deque[1] + deque[2];
            let second = deque[1] + deque[2] + deque[3];
            if second > first {
                larger += 1;
            }
            deque.pop_front();
        }
    }

    larger
}

pub fn day1(input: &str) -> (u64, u64) {
    let mut numbers: Vec<i32> = vec!();
    for line in utils::to_lines(input) {
        numbers.push(line.parse().unwrap());
    }

    (part1(&numbers), part2(&numbers))
}
