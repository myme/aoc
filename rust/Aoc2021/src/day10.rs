use std::collections::{HashMap, VecDeque};

use crate::utils;

fn find_corrupted_score(input: &String) -> Option<u64> {
    let chunk_chars: HashMap<char, char> = HashMap::from([
        ('(', ')'),
        ('[', ']'),
        ('{', '}'),
        ('<', '>'),
    ]);

    let scoring_table: HashMap<char, u64> = HashMap::from([
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

fn fix_incomplete_line(input: &String) -> Option<u64> {
    let chunk_chars: HashMap<char, char> = HashMap::from([
        ('(', ')'),
        ('[', ']'),
        ('{', '}'),
        ('<', '>'),
    ]);

    let scoring_table: HashMap<char, u64> = HashMap::from([
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

    if chunk_stack.is_empty() {
        return None
    }

    let mut sum = 0_u64;

    while let Some(closing) = chunk_stack.pop_front() {
        sum = sum * 5 + scoring_table.get(&closing)?;
    }

    return Some(sum);
}

pub fn day10(input: &str) -> (u64, u64) {
    let lines = utils::to_lines(input);
    let part1 = lines.iter().map(find_corrupted_score).flatten().sum();

    let mut fixed: Vec<u64> = lines.iter().filter_map(fix_incomplete_line).collect();
    fixed.sort();
    let part2 = fixed[fixed.len() / 2];

    (part1, part2)
}
