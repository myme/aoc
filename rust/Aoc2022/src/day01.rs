pub fn day1(input: &str) -> (String, String) {
    let mut sums = Vec::<_>::new();
    let mut sum = 0u64;

    for line in input.lines().map(|l| l.trim()) {
        if !line.is_empty() {
            sum += line.parse::<u64>().unwrap();
        } else {
            sums.push(sum);
            sum = 0;
        }
    }

    sums.push(sum);
    sums.sort_by(|a, b| b.cmp(a));

    let part1 = sums[0];
    let part2: u64 = sums[0..3].iter().sum();

    (part1.to_string(), part2.to_string())
}
