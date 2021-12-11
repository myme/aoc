use std::fs;

struct Motion<'a> {
    direction: &'a str,
    amount: u32,
}

fn part1(motions: &Vec<Motion>) -> u32 {
    let mut depth = 0;
    let mut position = 0;

    for motion in motions {
        match motion.direction {
            "forward" => { position += motion.amount },
            "down" => { depth += motion.amount },
            "up" => { depth -= motion.amount },
            _ => panic!("Invalid input"),
        };
    }

    depth * position
}

pub fn day2() {
    let input = fs::read_to_string("./input/day2.txt")
        .expect("Unable to read input file");

    let mut motions: Vec<Motion> = vec!();
    for line in input.split("\n") {
        if line.trim().is_empty() {
            continue;
        }

        let words: Vec<&str> = line.split(" ").collect();
        if words.len() != 2 {
            continue;
        }

        motions.push(Motion {
            direction: words[0],
            amount: words[1].parse().unwrap(),
        })
    }

    println!("Day 2");
    println!("  Part 1: {}", part1(&motions));
    println!("  Part 2");
}
