use std::collections::{HashMap, HashSet};

fn count_unique_digits(lines: &Vec<String>) -> i64 {
    let mut sum = 0;

    for line in lines {
        let parts: Vec<&str> = line.split("|").collect();
        sum += parts[1]
            .split(" ")
            .filter(|&v| -> bool {
                match v.len() {
                    2 => true,
                    3 => true,
                    4 => true,
                    7 => true,
                    _ => false,
                }
            })
            .count();
    }

    sum.try_into().unwrap()
}

type CharSet = HashSet<char>;

fn decode_and_compute_number(line: &String) -> i64 {
    let parts: Vec<&str> = line.split("|").collect();

    let outputs: Vec<CharSet> = parts[1]
        .split(" ")
        .filter(|v| !v.trim().is_empty())
        .map(|v| CharSet::from_iter(v.chars()))
        .collect();

    let digits: Vec<CharSet> = parts[0]
        .split(" ")
        .filter(|v| !v.trim().is_empty())
        .map(|v| CharSet::from_iter(v.chars()))
        .collect();

    let mut seg_counts: HashMap<char, i32> = HashMap::new();
    for digit in &digits {
        for ch in digit {
            let ch = seg_counts.entry(*ch).or_insert(0);
            *ch += 1;
        }
    }

    // Only need "four" to differentiate between segments based on frequency
    let four = digits.iter().find(|v| v.len() == 4).unwrap();

    // Segment frequency
    // a => 8, b => 6, c => 8, d => 7, e => 4, f => 9, g => 7
    let (a, _) = seg_counts.iter().find(|(k, &v)| v == 8 && !four.contains(k)).unwrap();
    let (b, _) = seg_counts.iter().find(|(_, &v)| v == 6).unwrap();
    let (c, _) = seg_counts.iter().find(|(k, &v)| v == 8 && four.contains(k)).unwrap();
    let (d, _) = seg_counts.iter().find(|(k, &v)| v == 7 && four.contains(k)).unwrap();
    let (e, _) = seg_counts.iter().find(|(_, &v)| v == 4).unwrap();
    let (f, _) = seg_counts.iter().find(|(_, &v)| v == 9).unwrap();
    let (g, _) = seg_counts.iter().find(|(k, &v)| v == 7 && !four.contains(k)).unwrap();

    let numbers = vec![
        (0, CharSet::from([*a, *b, *c, *e, *f, *g])),
        (1, CharSet::from([*c, *f])),
        (2, CharSet::from([*a, *c, *d, *e, *g])),
        (3, CharSet::from([*a, *c, *d, *f, *g])),
        (4, CharSet::from([*b, *c, *d, *f])),
        (5, CharSet::from([*a, *b, *d, *f, *g])),
        (6, CharSet::from([*a, *b, *d, *e, *f, *g])),
        (7, CharSet::from([*a, *c, *f])),
        (8, CharSet::from([*a, *b, *c, *d, *e, *f, *g])),
        (9, CharSet::from([*a, *b, *c, *d, *f, *g])),
    ];

    outputs.iter().enumerate().map(|(idx, chars)| {
        let pow = 10_i64.pow((3 - idx).try_into().unwrap());
        if let Some((num, _)) = numbers.iter().find(|(_, cs)| chars == cs) {
            num * pow
        } else {
            panic!("Not a number: {:?}", chars)
        }
    }).sum()
}

pub fn day8(lines: &Vec<String>) -> (i64, i64) {
    let part1 = count_unique_digits(lines);
    let part2 = lines.iter().map(decode_and_compute_number).sum();

    (part1, part2)
}
