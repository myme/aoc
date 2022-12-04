use crate::utils;

struct Input {
    min: u64,
    max: u64,
    positions: Vec<u64>,
}

fn parse_input(lines: &Vec<String>) -> Input {
    let mut min = 0;
    let mut max = 0;
    let mut positions: Vec<u64> = vec![];
    let mut first = true;

    for num_str in lines[0].split(",") {
        let num: u64 = num_str.parse().unwrap();
        if first {
            min = num;
            max = num;
            first = false;
        } else {
            min = min.min(num);
            max = max.max(num);
        }
        positions.push(num);
    }

    Input { min, max, positions }
}

fn calculate_fuel_usage(input: &Input, cost_increase: bool) -> u64 {
    let Input { min, max, positions } = input;

    let mut fuel = 0;
    let mut first = true;

    for alignment in *min..=*max {
        let mut sum = 0;

        for p in positions {
            let moves = if alignment > *p { alignment - p } else { p - alignment };
            let diff = if cost_increase {
                ((moves as f64 / 2.0) * (1.0 + moves as f64)) as u64
            } else {
                moves
            };
            sum += diff;
        }

        if first {
            fuel = sum;
            first = false;
        } else {
            fuel = fuel.min(sum);
        }
    }

    fuel
}

pub fn day7(input: &str) -> (u64, u64) {
    let input  = parse_input(&utils::to_lines(input));

    let part1 = calculate_fuel_usage(&input, false);
    let part2 = calculate_fuel_usage(&input, true);

    (part1, part2)
}
