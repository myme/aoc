use crate::utils;

enum Direction {
    Forward,
    Down,
    Up,
}

struct Motion {
    direction: Direction,
    amount: i32,
}

fn part1(motions: &Vec<Motion>) -> i32 {
    let mut depth = 0;
    let mut position = 0;

    for motion in motions {
        match &motion.direction {
            Direction::Forward => { position += motion.amount },
            Direction::Down => { depth += motion.amount },
            Direction::Up => { depth -= motion.amount },
        };
    }

    depth * position
}

fn part2(motions: &Vec<Motion>) -> i32 {
    let mut aim: i32 = 0;
    let mut depth: i32 = 0;
    let mut position = 0;

    for motion in motions {
        match &motion.direction {
            Direction::Forward => {
                position += motion.amount;
                depth += motion.amount * aim;
            },
            Direction::Down => { aim += motion.amount },
            Direction::Up => { aim -= motion.amount },
        };
    }

    depth * position
}

pub fn day2(input: &str) -> (i64, i64) {
    let mut motions: Vec<Motion> = vec!();
    for line in utils::to_lines(input) {
        let words: Vec<&str> = line.split(" ").collect();
        if words.len() != 2 {
            continue;
        }

        let dir = match words[0] {
            "forward" => Direction::Forward,
            "down" => Direction::Down,
            "up" => Direction::Up,
            _ => panic!("Invalid input"),
        };

        motions.push(Motion {
            direction: dir,
            amount: words[1].parse().unwrap(),
        })
    }

    (part1(&motions).into(), part2(&motions).into())
}
