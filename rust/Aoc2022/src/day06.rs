use std::collections::HashSet;

fn find_distinct(input: &str, n_distinct: usize) -> String {
    let chars = input.trim().chars().collect::<Vec<_>>();

    for (idx, each) in chars.windows(n_distinct).enumerate() {
        let set: HashSet<&char> = HashSet::from_iter(each.iter());
        if set.len() == n_distinct {
            return (idx + n_distinct).to_string();
        }
    }

    unreachable!("No marker!")
}

pub fn day6(input: &str) -> (String, String) {
    let part1 = find_distinct(input, 4);
    let part2 = find_distinct(input, 14);

    (part1, part2)
}
