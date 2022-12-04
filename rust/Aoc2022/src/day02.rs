use crate::utils;

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord)]
enum Hand {
    Rock,
    Paper,
    Scissors,
}

fn parse_type(input: &str) -> Option<Hand> {
    match input {
        "A" => Some(Hand::Rock),
        "X" => Some(Hand::Rock),
        "B" => Some(Hand::Paper),
        "Y" => Some(Hand::Paper),
        "C" => Some(Hand::Scissors),
        "Z" => Some(Hand::Scissors),
        _ => None,
    }
}

fn relation(hand: &Hand) -> (Hand, Hand) {
    match hand {
        Hand::Rock => (Hand::Paper, Hand::Scissors),
        Hand::Paper => (Hand::Scissors, Hand::Rock),
        Hand::Scissors => (Hand::Rock, Hand::Paper),
    }
}

fn find_hand(hand: &Hand, result: &str) -> Option<Hand> {
    match result {
        "X" => Some(relation(hand).1),
        "Y" => Some(hand.clone()),
        "Z" => Some(relation(hand).0),
        _ => None,
    }
}

fn score_hand(hand: &Hand) -> i64 {
    match hand {
        Hand::Rock => 1,
        Hand::Paper => 2,
        Hand::Scissors => 3,
    }
}

fn score_win(hand: &Hand, other: &Hand) -> i64 {
    match relation(hand) {
        (lose, _) if *other == lose => 0,
        (_, win) if *other == win => 6,
        _ => 3,
    }
}

fn part1(input: &str) -> Option<i64> {
    let mut score: i64 = 0;

    for line in utils::to_lines(input) {
        let mut parts = line.split(" ").filter_map(parse_type);
        let opponent: Hand = parts.next()?;
        let own: Hand = parts.next()?;
        score += score_hand(&own) + score_win(&own, &opponent)
    }

    Some(score)
}

fn part2(input: &str) -> Option<i64> {
    let mut score: i64 = 0;

    for line in utils::to_lines(input) {
        let mut parts = line.split(" ");
        let opponent: Hand = parts.next().and_then(parse_type)?;
        let own: Hand = parts.next().and_then(|result| find_hand(&opponent, result))?;
        score += score_hand(&own) + score_win(&own, &opponent)
    }

    Some(score)
}

pub fn day2(input: &str) -> (i64, i64) {
    (part1(input).unwrap_or(0), part2(input).unwrap_or(0))
}
