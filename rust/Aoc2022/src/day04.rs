fn part1(input: &str) -> Option<u64> {
    let mut count = 0u64;

    for line in input.lines() {
        // let mut splits = line.split(",");
        // splits.next()?.split("-");
        // splits.next()?;
        match line[..] {
            ["a"] => 0,
        };
    }

    Some(count)
}

pub fn day4(input: &str) -> (u64, u64) {
    (part1(input).unwrap_or(0), 0)
}
