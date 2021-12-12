mod day1;
mod day2;

fn run_day(day: u32, func: fn() -> (i32, i32)) {
    let (part1, part2) = func();
    println!("Day {}", day);
    println!("  Part 1: {}", part1);
    println!("  Part 2: {}", part2);
}

fn main() {
    run_day(1, day1::day1);
    run_day(2, day2::day2);
}
