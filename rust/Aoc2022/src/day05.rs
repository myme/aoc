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

type Op = (usize, usize, usize);

// Parse: "move 1 from 2 to 3" => (1, 2, 3)
fn parse_op<'a, I>(words: I) -> Option<Op>
where
    I: IntoIterator<Item = &'a str>,
{
    let mut iter = words
        .into_iter()
        .filter_map(|each| each.parse().ok());
    let count = iter.next()?;
    let from = iter.next()?;
    let to = iter.next()?;
    Some((count, from - 1, to - 1))
}

type Stacks = Vec<Vec<char>>;

fn parse_state(input: &str) -> Stacks {
    let mut ls = input.rsplit('\n');

    // Parse: " 1   2   3   .."
    let n_stacks = ls
        .next()
        .unwrap()
        .split(' ')
        .filter(|each| !each.trim().is_empty())
        .count();
    let mut stacks: Vec<Vec<char>> = vec![Vec::new(); n_stacks];

    // Parse: "[Z] [M] [P]"
    for line in ls {
        let mut chars = line.chars();
        for stack in &mut stacks {
            if let Some(char) = parse_char(&mut chars) {
                stack.push(char);
            }
        }
    }

    stacks
}

fn run<F>(input: &str, handle_op: F) -> Option<String>
where F: Fn(Op, &mut Stacks)
{
    let mut parts = input.split("\n\n");
    let mut stacks = parse_state(parts.next()?);

    for line in parts.next()?.lines() {
        let words = line.split(' ').filter(|each| !each.trim().is_empty());
        if let Some((count, from, to)) = parse_op(words) {
            handle_op((count, from, to), &mut stacks);
        }
    }

    Some(stacks.iter().filter_map(|stack| stack.last()).collect::<String>())
}

pub fn day5(input: &str) -> (String, String) {
    let part1 = run(input, |(count, from, to), stacks| {
        for _ in 0 .. count {
            if let Some(val) = stacks[from].pop() {
                stacks[to].push(val);
            }
        }
    });

    let part2 = run(input, |(count, from, to), stacks| {
        let from_len = stacks[from].len();
        let drained = stacks[from]
            .drain(from_len - count .. from_len)
            .collect::<Vec<_>>();
        for each in drained {
            stacks[to].push(each);
        }
    });

    (part1.unwrap_or_default(), part2.unwrap_or_default())
}
