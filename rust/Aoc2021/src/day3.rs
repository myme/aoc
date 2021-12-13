fn from_binary_string(input: &str) -> i32 {
    i32::from_str_radix(input, 2).expect(&format!("Unable to parse binary number: {}", input))
}

fn get_bit_frequencies(lines: &[String]) -> Vec<i32> {
    if lines.is_empty() {
        return vec![];
    }

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

    counters
}

fn part1(lines: &Vec<String>) -> i32 {
    let counters = get_bit_frequencies(lines);

    let mut gamma_str = String::from("");
    let mut epsilon_str = String::from("");

    for c in counters {
        let (g, e) = if c > 0 { ('1', '0') } else { ('0', '1') };
        gamma_str.push(g);
        epsilon_str.push(e);
    }

    let gamma = from_binary_string(&gamma_str);
    let epsilon = from_binary_string(&epsilon_str);

    gamma * epsilon
}

type Branch = Option<Box<BinTrie>>;
struct BinTrie {
    count: i32,
    zero: Branch,
    one: Branch,
}

fn reify_branch(branch: &mut Branch) -> &mut BinTrie {
    if let None = branch {
        let new = Box::new(BinTrie {
            count: 0,
            zero: None,
            one: None,
        });

        *branch = Some(new);
    }

    match branch {
        Some(b) => b.as_mut(),
        None => panic!("Branch cannot be None at this point"),
    }
}

fn build_trie(lines: &Vec<String>) -> BinTrie {
    let mut trie = BinTrie {
        count: 0,
        zero: None,
        one: None,
    };

    for line in lines {
        let mut node = &mut trie;

        for bit in line.chars() {
            node.count += 1;
            match bit {
                '0' => node = reify_branch(&mut node.zero),
                '1' => node = reify_branch(&mut node.one),
                _ => panic!("Invalid line bit {} in {}", bit, line),
            }
        }
    }

    trie
}

#[allow(dead_code)]
fn print_trie(trie: &BinTrie, prefix: &str) {
    println!("Prefix: {}, Nodes: {}", prefix, trie.count);

    if let Some(p) = &trie.zero {
        print_trie(p, &format!("{}0", prefix))
    }

    if let Some(p) = &trie.one {
        print_trie(p, &format!("{}1", prefix))
    }
}

enum Direction {
    Left,
    Right,
}

fn find_node_path<F>(trie: &BinTrie, prefix: &str, pred: F) -> String
where
    F: Fn(&BinTrie) -> Direction,
{
    match pred(trie) {
        Direction::Left => {
            if let Some(b) = &trie.zero {
                find_node_path(b, &format!("{}0", prefix), pred)
            } else if let Some(b) = &trie.one {
                find_node_path(b, &format!("{}1", prefix), pred)
            } else {
                String::from(prefix)
            }
        }
        Direction::Right => {
            if let Some(b) = &trie.one {
                find_node_path(b, &format!("{}1", prefix), pred)
            } else if let Some(b) = &trie.zero {
                find_node_path(b, &format!("{}0", prefix), pred)
            } else {
                String::from(prefix)
            }
        }
    }
}

fn find_node_value_path<F>(trie: &BinTrie, prefix: &str, pred: F) -> String
where
    F: Fn(i32, i32) -> Direction,
{
    find_node_path(trie, prefix, |branch| {
        let zeroes = branch.zero.as_ref().map_or(0, |branch| branch.count);
        let ones = branch.one.as_ref().map_or(0, |branch| branch.count);
        pred(zeroes, ones)
    })
}

fn part2(trie: &BinTrie) -> i32 {
    let o2_gen_rating_str = find_node_value_path(&trie, "", |zero, one| {
        if zero > one {
            Direction::Left
        } else {
            Direction::Right
        }
    });
    let co2_scrub_rating_str = find_node_value_path(&trie, "", |zero, one| {
        if zero <= one {
            Direction::Left
        } else {
            Direction::Right
        }
    });

    let o2_gen_rating = from_binary_string(&o2_gen_rating_str);
    let co2_scrub_rating = from_binary_string(&co2_scrub_rating_str);

    o2_gen_rating * co2_scrub_rating
}

pub fn day3(lines: &Vec<String>) -> (i32, i32) {
    let trie = build_trie(lines);
    // print_trie(&trie, "");

    (part1(lines), part2(&trie))
}
