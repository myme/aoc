use std::{collections::VecDeque, fs};

fn part1(numbers: &Vec<i32>) -> i32 {
    let mut larger: i32 = 0;
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

fn part2(numbers: &Vec<i32>) -> i32 {
    let mut larger: i32 = 0;
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

pub fn day1() -> (i32, i32) {
    let input = fs::read_to_string("./input/day1.txt")
        .expect("Unable to read input file");

    let mut numbers: Vec<i32> = vec!();
    for line in input.split("\n") {
        if line.trim().is_empty() {
            continue;
        }
        numbers.push(line.parse().unwrap());
    }

    (part1(&numbers), part2(&numbers))
}
