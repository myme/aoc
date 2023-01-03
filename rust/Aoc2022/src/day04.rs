use std::collections::HashSet;

struct Range {
    start: u64,
    end: u64,
}

impl Range {
    fn from_str(input: &str) -> Option<Range> {
        let mut iter = input.split('-').filter_map(|each| each.parse().ok());
        let start = iter.next()?;
        let end = iter.next()?;
        Some(Range { start, end })
    }

    fn contains(&self, Range { start, end }: &Range) -> bool {
        *start >= self.start && *end <= self.end
    }

    fn intersects(&self, other: &Range) -> bool {
        let first = HashSet::<u64>::from_iter(self.start..=self.end);
        let second = HashSet::<u64>::from_iter(other.start..=other.end);
        first.intersection(&second).count() > 0
    }
}

fn solve<F>(input: &str, pred: F) -> usize
where
    F: Fn(&Range, &Range) -> bool,
{
    input
        .lines()
        .filter(|line| {
            let mut iter = line.split(',').filter_map(Range::from_str);
            match (iter.next(), iter.next()) {
                (Some(first), Some(second)) => pred(&first, &second),
                _ => false,
            }
        })
        .count()
}

pub fn day4(input: &str) -> (String, String) {
    let part1 = solve(input, |first, second| {
        first.contains(second) || second.contains(first)
    });
    let part2 = solve(input, |first, second| first.intersects(second));
    (part1.to_string(), part2.to_string())
}
