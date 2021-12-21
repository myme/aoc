use std::collections::{HashMap, VecDeque};

fn find_corrupted_score(input: &String) -> Option<i64> {
    let chunk_chars: HashMap<char, char> = HashMap::from([
        ('(', ')'),
        ('[', ']'),
        ('{', '}'),
        ('<', '>'),
    ]);

    let scoring_table: HashMap<char, i64> = HashMap::from([
        (')', 3),
        (']', 57),
        ('}', 1197),
        ('>', 25137),
    ]);

    let mut chunk_stack = VecDeque::new();

    for chr in input.chars() {
        if let Some(closing) = chunk_chars.get(&chr) {
            chunk_stack.push_front(*closing);
        } else if let Some(closing) = chunk_stack.pop_front() {
            if chr != closing {
                return scoring_table.get(&chr).map(|v| *v);
            }
        }
    }

    None
}

pub fn day10(lines: &Vec<String>) -> (i64, i64) {
    let part1 = lines.iter().map(find_corrupted_score).flatten().sum();
    (part1, 0)
}
