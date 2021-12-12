fn part1(lines: &Vec<String>) -> i32 {
    let length = lines[0].len();
    let mut counters = vec![0; length];

    for line in lines {
        if line.len() != length {
            panic!("Invalid line length, expecting {}: {}", length, line);
        }

        for (idx, bit) in line.chars().enumerate() {
            match bit {
                '0' => counters[idx] -= 1,
                '1' => counters[idx] += 1,
                _ => panic!("Invalid line bit {} in {}", bit, line),
            }
        }
    }

    let mut gamma_str = String::from("");
    let mut epsilon_str = String::from("");

    for c in counters {
        let (g, e) = if c > 0 { ('1', '0') } else { ('0', '1') };
        gamma_str.push(g);
        epsilon_str.push(e);
    }

    let gamma = i32::from_str_radix(&gamma_str, 2)
        .expect(&format!("Unable to parse binary number: {}", gamma_str));
    let epsilon = i32::from_str_radix(&epsilon_str, 2)
        .expect(&format!("Unable to parse binary number: {}", epsilon_str));

    gamma * epsilon
}

fn part2(_lines: &Vec<String>) -> i32 {
    0
}

pub fn day3(lines: &Vec<String>) -> (i32, i32) {
    (part1(lines), part2(lines))
}
