type Fish = [i64; 9];

pub fn breeding_program(lines: &Vec<String>, days: u32) -> Fish {
    let mut fish: Fish = [0; 9];

    for fish_cycle in lines[0].split(',') {
        let idx: usize = fish_cycle.parse().unwrap();
        fish[idx] += 1;
    }

    for _day in 1..=days {
        // println!("day: {}, fish: {:?}", _day, fish);

        let mut new_fish: Fish = [0; 9];

        for (fish_cycle, count) in fish.iter().enumerate() {
            if fish_cycle == 0 {
                new_fish[6] = fish[fish_cycle];
                new_fish[8] = *count;
            } else {
                new_fish[fish_cycle - 1] += fish[fish_cycle];
            }
        }

        fish = new_fish;
    }

    fish
}

pub fn day6(lines: &Vec<String>) -> (i64, i64) {
    let part1 = breeding_program(&lines, 80).iter().sum();
    let part2 = breeding_program(&lines, 256).iter().sum();

    (part1, part2)
}
