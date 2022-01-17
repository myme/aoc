use std::collections::VecDeque;

use crate::utils;

type Coord = (usize, usize);
type State = Vec<Vec<u8>>;

fn parse_octopuses(lines: &Vec<String>) -> State {
    let mut state = Vec::new();

    for line in lines {
        let row = line.chars().map(|c| {
            if !c.is_numeric() { panic!("Invalid input: {}", c); }
            c as u8 - 48
        }).collect();
        state.push(row);
    }

    state
}

fn neighbor_cells(row: usize, col: usize, (rows, cols): Coord) -> Vec<Coord> {
    let mut neighbors = Vec::new();

    if row > 0 {
        if col > 0 {
            neighbors.push((row - 1, col - 1));
        }
        neighbors.push((row - 1, col));
        if col < cols - 1 {
            neighbors.push((row - 1, col + 1));
        }
    }

    if col > 0 {
        neighbors.push((row, col - 1));
    }

    if col < cols - 1 {
        neighbors.push((row, col + 1));
    }

    if row < rows - 1 {
        if col > 0 {
            neighbors.push((row + 1, col - 1));
        }
        neighbors.push((row + 1, col));
        if col < cols - 1 {
            neighbors.push((row + 1, col + 1));
        }
    }

    neighbors
}

enum Part {
    Part1,
    Part2,
}

fn run_simulation(part: Part, mut state: State) -> i64 {
    let mut step = 0;
    let mut total_flashes = 0_i64;
    let rows = state.len();
    let cols = state[0].len();

    loop {
        let mut flashes_in_step = 0;
        let mut flashes: VecDeque<(usize, usize)> = VecDeque::new();

        for row in 0..state.len() {
            for col in 0..state[0].len() {
                state[row][col] += 1;
                if state[row][col] >= 10 {
                    flashes.push_back((row, col));
                }
            }
        }

        while let Some((row, col)) = flashes.pop_front() {
            if state[row][col] == 0 {
                continue;
            }

            state[row][col] = 0;
            flashes_in_step += 1;

            for (r, c) in neighbor_cells(row, col, (rows, cols)) {
                if state[r][c] == 0 {
                    continue;
                }

                state[r][c] += 1;

                if state[r][c] >= 10 {
                    flashes.push_back((r, c));
                }
            }
        }

        total_flashes += flashes_in_step;
        step += 1;

        match part {
            Part::Part1 => if step == 100 { return total_flashes },
            Part::Part2 => if flashes_in_step == 100 { return step },
        }
    }
}

pub fn day11(input: &str) -> (i64, i64) {
    let lines = &utils::to_lines(input);
    let part1 = run_simulation(Part::Part1, parse_octopuses(lines));
    let part2 = run_simulation(Part::Part2, parse_octopuses(lines));

    (part1, part2)
}
