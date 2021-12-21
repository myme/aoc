type Grid = Vec<Vec<u8>>;

fn parse_grid(lines: &Vec<String>) -> Grid {
    let mut grid: Vec<Vec<u8>> = Vec::new();

    for line in lines {
        let row: Vec<u8> = line.chars().map(|v| (v as u8) - 0x30).collect();
        grid.push(row);
    }

    grid
}

fn find_low_point_sum(lines: &Vec<String>) -> i64 {
    let grid = parse_grid(lines);
    let height = grid.len();
    let width = grid[0].len();

    let mut sum = 0;

    for (row_idx, row) in grid.iter().enumerate() {
        for (col_idx, depth) in row.iter().enumerate() {
            if row_idx > 0 && depth >= &grid[row_idx - 1][col_idx] {
                continue;
            }
            if col_idx > 0 && depth >= &grid[row_idx][col_idx - 1] {
                continue;
            }
            if row_idx < height - 1 && depth >= &grid[row_idx + 1][col_idx] {
                continue;
            }
            if col_idx < width - 1 && depth >= &grid[row_idx][col_idx + 1] {
                continue;
            }
            sum += 1 + i64::from(*depth);
        }
    }

    sum
}

pub fn day9(lines: &Vec<String>) -> (i64, i64) {
    let part1 = find_low_point_sum(lines);

    (part1, 0)
}
