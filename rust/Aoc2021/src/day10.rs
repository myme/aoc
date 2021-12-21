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

fn fix_incomplete_line(input: &String) -> Option<i64> {
    let chunk_chars: HashMap<char, char> = HashMap::from([
        ('(', ')'),
        ('[', ']'),
        ('{', '}'),
        ('<', '>'),
    ]);

    let scoring_table: HashMap<char, i64> = HashMap::from([
        (')', 1),
        (']', 2),
        ('}', 3),
        ('>', 4),
    ]);

    let mut chunk_stack = VecDeque::new();

    for chr in input.chars() {
        if let Some(closing) = chunk_chars.get(&chr) {
            chunk_stack.push_front(*closing);
        } else if let Some(closing) = chunk_stack.pop_front() {
            if chr != closing {
                return None
            }
        }
    }

    if !chunk_stack.is_empty() {
        let mut sum = 0_i64;

        while let Some(closing) = chunk_stack.pop_front() {
            sum = sum * 5 + scoring_table.get(&closing)?;
        }

        return Some(sum);
    }

    None
}

pub fn day10(lines: &Vec<String>) -> (i64, i64) {
    let part1 = lines.iter().map(find_corrupted_score).flatten().sum();

    let mut fixed: Vec<i64> = lines.iter().filter_map(fix_incomplete_line).collect();
    fixed.sort();
    let part2 = fixed[fixed.len() / 2];

    (part1, part2)
}
