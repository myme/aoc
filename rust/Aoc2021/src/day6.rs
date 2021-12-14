pub fn breeding_program(lines: &Vec<String>, days: u32) -> Vec<u32> {
    let mut fish: Vec<u32> = lines[0].split(',').map(|v| v.parse().unwrap()).collect();

    for _day in 1..=days {
        // println!("day: {}, fish: {:?}", _day, fish);

        let mut new_fish = Vec::<u32>::new();

        for f in &mut fish {
            if *f == 0 {
                *f = 6;
                new_fish.push(8)
            } else {
                *f -= 1;
            }
        }

        fish.extend(new_fish);
    }

    fish
}

pub fn day6(lines: &Vec<String>) -> (i32, i32) {
    let part1 = breeding_program(&lines, 80).len();
    let part2 = 0; // breeding_program(&lines, 256).len();

    (part1.try_into().unwrap(), 0)
}
