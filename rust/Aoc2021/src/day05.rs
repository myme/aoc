use std::collections::{HashMap, VecDeque};

use crate::utils;

type Point = (i32, i32);

#[derive(Debug)]
struct Line {
    start: Point,
    stop: Point,
}

fn parse_lines(input: &Vec<String>) -> Vec<Line> {
    let mut lines: Vec<Line> = vec![];

    for line in input {
        let mut points: VecDeque<Point> = line
            .split("->")
            .map(|x| {
                let coords: Vec<i32> = x
                    .trim()
                    .split(",")
                    .map(|n| n.parse().unwrap())
                    .collect();
                (coords[0], coords[1])
            })
            .collect();
        let start = points.pop_front();
        let stop = points.pop_front();
        match (start, stop) {
            (Some(start), Some(stop)) => lines.push(Line { start, stop }),
            _ => panic!("Invalid line coordinates: {}", line),
        }
    }

    lines
}


fn build_map(lines: &Vec<Line>, with_diagonal: bool) -> HashMap<Point, u64> {
    let mut map = HashMap::new();

    for line in lines {
        let Line {
            start: (start_x, start_y),
            stop: (stop_x, stop_y),
        } = *line;

        let is_diagonal = start_x != stop_x && start_y != stop_y;
        if !with_diagonal && is_diagonal {
            continue
        }

        let mut x  = start_x;
        let mut y  = start_y;
        let step_x = if start_x > stop_x { -1 } else { 1 };
        let step_y = if start_y > stop_y { -1 } else { 1 };

        loop {
            let count = map.entry((x, y)).or_insert(0);
            *count += 1;
            if (x, y) == (stop_x, stop_y) {
                break;
            }
            if x != stop_x {
                x += step_x;
            }
            if y != stop_y {
                y += step_y;
            }
        }
    }

    map
}

pub fn day5(input: &str) -> (u64, u64) {
    let lines = parse_lines(&utils::to_lines(input));

    let map = build_map(&lines, false);
    let part1 = map.values().filter(|&v| *v >= 2).count();

    let map = build_map(&lines, true);
    let part2 = map.values().filter(|&v| *v >= 2).count();

    (part1.try_into().unwrap(), part2.try_into().unwrap())
}
