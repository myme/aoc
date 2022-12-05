use std::{collections::HashSet, iter::Sum};

fn to_priority(chr: char) -> u64 {
    if chr.is_lowercase() {
        (chr as u64) - 0x60
    } else {
        (chr as u64) - 38
    }
}

fn part1(input: &str) -> u64 {
    let mut sum = 0u64;

    for line in input.lines() {
        let (fst, snd) = line.split_at(line.len() / 2);

        let fst_pris = HashSet::<u64>::from_iter(fst.chars().map(to_priority));
        let snd_pris = HashSet::<u64>::from_iter(snd.chars().map(to_priority));

        for each in fst_pris.intersection(&snd_pris) {
            sum += *each;
        }
    }

    sum
}

fn part2(input: &str) -> u64 {
    let all_chars = HashSet::<char>::from_iter(('a'..='z').chain('A'..='Z'));
    let lines = input.lines().collect::<Vec<&str>>();
    let mut sum = 0u64;

    for chunks in lines.chunks(3) {
        let mut set = all_chars.clone();

        for each in chunks {
            let other = HashSet::<char>::from_iter(each.chars());
            let is = set.intersection(&other);
            set = is.map(|c| *c).collect::<HashSet<char>>();
        }

        sum += u64::sum(set.iter().map(|c| to_priority(*c)));
    }

    sum
}

pub fn day3(input: &str) -> (u64, u64) {
    (part1(input), part2(input))
}
