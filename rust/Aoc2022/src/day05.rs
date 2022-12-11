use std::str::Chars;

// Parse each of: "[Z] [M] [P]"
fn parse_char(chars: &mut Chars) -> Option<char> {
    chars.next()?;
    let char = chars.next()?;
    chars.next()?;
    chars.next();
    if char.is_whitespace() {
        None
    } else {
        Some(char)
    }
}

// Parse: "move 1 from 2 to 3" => (1, 2, 3)
fn parse_op<'a, I>(words: I) -> Option<(usize, usize, usize)>
where
    I: IntoIterator<Item = &'a str>,
{
    let mut iter = words
        .into_iter()
        .filter_map(|each| each.parse().ok());
    let count = iter.next()?;
    let from = iter.next()?;
    let to = iter.next()?;
    Some((count, from, to))
}

fn parse_state(input: &str) -> Vec<Vec<char>> {
    let mut ls = input.rsplit("\n");

    // Parse: " 1   2   3   .."
    let n_stacks = ls
        .next()
        .unwrap()
        .split(" ")
        .filter(|each| !each.trim().is_empty())
        .count();
    let mut stacks: Vec<Vec<char>> = vec![Vec::new(); n_stacks];

    // Parse: "[Z] [M] [P]"
    for line in ls {
        let mut chars = line.chars();
        for i in 0..n_stacks {
            if let Some(char) = parse_char(&mut chars) {
                stacks[i].push(char);
            }
        }
    }

    stacks
}

fn exec_ops(input: &str, stacks: &mut Vec<Vec<char>>) {
    for line in input.lines() {
        let words = line.split(" ").filter(|each| !each.trim().is_empty());
        if let Some((count, from, to)) = parse_op(words) {
            for _ in 0 .. count {
                if let Some(val) = stacks[from - 1].pop() {
                    stacks[to - 1].push(val);
                }
            }
        }
    }
}

fn part1(input: &str) -> Option<String> {
    let mut parts = input.split("\n\n");

    let mut stacks = parse_state(parts.next()?);
    exec_ops(parts.next()?, &mut stacks);

    Some(stacks.iter().filter_map(|stack| stack.last()).collect::<String>())

    // Some(String::new())
}

pub fn day5(input: &str) -> (String, String) {
    (part1(input).unwrap_or(String::new()), String::from(""))
}
