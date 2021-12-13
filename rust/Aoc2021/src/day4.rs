type Board = [(i32, bool); 25];

fn parse_boards(lines: &[String]) -> Vec<Board> {
    let mut boards = vec!();
    let mut board: Board = [(0, false); 25];

    for (line_idx, line) in lines.iter().enumerate() {
        for (word_idx, word) in line.split(' ').filter(|w| !w.is_empty()).enumerate() {
            let idx = (line_idx % 5) * 5 + word_idx;
            board[idx] = (word.parse().unwrap(), false);

            if idx == 24 {
                boards.push(board);
                board = [(0, false); 25];
            }
        }
    }

    boards
}

fn check_win(board: &Board) -> bool {
    'rows: for line_idx in 0..5 {
        for word_idx in 0..5 {
            if !board[line_idx * 5 + word_idx].1 {
                continue 'rows;
            }
        }
        return true;
    }

    'columns: for word_idx in 0..5 {
        for line_idx in 0..5 {
            if !board[line_idx * 5 + word_idx].1 {
                continue 'columns;
            }
        }
        return true;
    }

    false
}

fn sum_board_unchecked(board: &Board) -> i32 {
    let mut sum = 0;

    for (value, checked) in board {
        if !*checked {
            sum += value;
        }
    }

    sum
}

fn play_bingo(boards: &mut Vec<Board>, draws: &Vec<i32>) -> i32 {
    for draw in draws {
        for board in &mut *boards {
            for cell in &mut *board {
                if cell.0 == *draw {
                    cell.1 = true;
                }
            }
            if check_win(&board) {
                println!("WIN! {}", draw);
                return sum_board_unchecked(&board) * draw;
            }
        }
    }

    0
}

pub fn day4(lines: &Vec<String>) -> (i32, i32) {
    let draws: Vec<i32> = lines[0].split(',').map(|v| v.parse().unwrap()).collect();
    let mut boards = parse_boards(&lines[1..]);
    let part1 = play_bingo(&mut boards, &draws);
    (part1, 0)
}
