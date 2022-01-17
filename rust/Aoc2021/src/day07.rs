struct Input {
    min: i64,
    max: i64,
    positions: Vec<i64>,
}

fn parse_input(lines: &Vec<String>) -> Input {
    let mut min = 0;
    let mut max = 0;
    let mut positions: Vec<i64> = vec![];
    let mut first = true;

    for num_str in lines[0].split(",") {
        let num: i64 = num_str.parse().unwrap();
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

fn calculate_fuel_usage(input: &Input, cost_increase: bool) -> i64 {
    let Input { min, max, positions } = input;

    let mut fuel = 0;
    let mut first = true;

    for alignment in *min..=*max {
        let mut sum = 0;

        for p in positions {
            let moves = if alignment > *p { alignment - p } else { p - alignment };
            let diff = if cost_increase {
                ((moves as f64 / 2.0) * (1.0 + moves as f64)) as i64
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

pub fn day7(lines: &Vec<String>) -> (i64, i64) {
    let input  = parse_input(lines);

    let part1 = calculate_fuel_usage(&input, false);
    let part2 = calculate_fuel_usage(&input, true);

    (part1, part2)
}
