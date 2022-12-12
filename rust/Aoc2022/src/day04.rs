use std::collections::HashSet;

type Pair = (u64, u64);

fn to_pair(input: &str) -> Pair {
    let mut iter = input.split('-').map(|each| each.parse().unwrap());
    let start = iter.next().unwrap();
    let end = iter.next().unwrap();
    (start, end)
}

fn is_contained((x1, x2): Pair, (y1, y2): Pair) -> bool {
    (x1 <= y1 && x2 >= y2) || (x1 >= y1 && x2 <= y2)
}

fn part1(input: &str) -> u64 {
    let mut count = 0u64;

    for line in input.lines() {
        let mut iter = line.split(',').map(to_pair);
        let first = iter.next().unwrap();
        let second = iter.next().unwrap();
        if is_contained(first, second) {
            count += 1;
        }
    }

    count
}

fn part2(input: &str) -> u64 {
    let mut count = 0u64;

    for line in input.lines() {
        let mut iter = line
            .split(',')
            .map(to_pair)
            .map(|(start, end)| HashSet::<u64>::from_iter(start..=end));
        let first = iter.next().unwrap();
        let second = iter.next().unwrap();
        if first.intersection(&second).count() > 0 {
            count += 1;
        }
    }

    count
}

pub fn day4(input: &str) -> (String, String) {
    (part1(input).to_string(), part2(input).to_string())
}
