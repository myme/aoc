use std::collections::HashSet;

fn part1(input: &str) -> String {
    let chars = input.trim().chars().collect::<Vec<_>>();

    for (idx, each) in chars.windows(4).enumerate() {
        let set: HashSet<&char> = HashSet::from_iter(each.iter());
        if set.len() == 4 {
            return (idx + 4).to_string();
        }
    }

    String::new()
}

pub fn day6(input: &str) -> (String, String) {
    (part1(input), String::new())
}
