fn part1(sums: &Vec<i64>) -> i64 {
    sums[0]
}

fn part2(sums: &Vec<i64>) -> i64 {
    sums[0] + sums[1] + sums[2]
}

pub fn day1(input: &str) -> (i64, i64) {
    let mut sums = Vec::<i64>::new();
    let mut sum: i64 = 0;

    for line in input.lines().map(|l| l.trim()) {
        if !line.is_empty() {
            sum += line.parse::<i64>().unwrap();
        } else {
            sums.push(sum);
            sum = 0;
        }
    }

    sums.push(sum);
    sums.sort_by(|a, b| b.cmp(a));

    (part1(&sums), part2(&sums))
}
