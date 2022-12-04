fn part1(sums: &Vec<u64>) -> u64 {
    sums[0]
}

fn part2(sums: &Vec<u64>) -> u64 {
    sums[0] + sums[1] + sums[2]
}

pub fn day1(input: &str) -> (u64, u64) {
    let mut sums = Vec::<u64>::new();
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

    (part1(&sums), part2(&sums))
}
