use std::collections::HashSet;

fn to_priority(chr: char) -> u64 {
    if chr.is_lowercase() {
        (chr as u64) - 0x60
    } else {
        (chr as u64) - 38
    }
}

fn part1(input: &str) -> i64 {
    let mut sum = 0i64;

    for line in input.lines() {
        let (fst, snd) = line.split_at(line.len() / 2);

        let fst_pri = HashSet::<u64>::from_iter(fst.chars().map(to_priority));
        let snd_pri = HashSet::<u64>::from_iter(snd.chars().map(to_priority));

        for each in fst_pri.intersection(&snd_pri) {
            sum += *each as i64;
        }
    }

    sum
}

pub fn day3(input: &str) -> (i64, i64) {
    (part1(input), 0)
}
