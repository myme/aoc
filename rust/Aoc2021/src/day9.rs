type Grid = Vec<Vec<u8>>;

fn parse_grid(lines: &Vec<String>) -> Grid {
    let mut grid: Vec<Vec<u8>> = Vec::new();

    for line in lines {
        let row: Vec<u8> = line.chars().map(|v| (v as u8) - 0x30).collect();
        grid.push(row);
    }

    grid
}

#[allow(dead_code)]
struct Point {
    row: usize,
    col: usize,
    height: u8,
}

fn find_low_points(grid: &Grid) -> Vec<Point> {
    let mut points = Vec::new();
    let grid_height = grid.len();
    let grid_width = grid[0].len();

    for (row_idx, row) in grid.iter().enumerate() {
        for (col_idx, height) in row.iter().enumerate() {
            if row_idx > 0 && height >= &grid[row_idx - 1][col_idx] {
                continue;
            }
            if col_idx > 0 && height >= &grid[row_idx][col_idx - 1] {
                continue;
            }
            if row_idx < grid_height - 1 && height >= &grid[row_idx + 1][col_idx] {
                continue;
            }
            if col_idx < grid_width - 1 && height >= &grid[row_idx][col_idx + 1] {
                continue;
            }
            points.push(Point {
                row: row_idx,
                col: col_idx,
                height: *height,
            });
        }
    }

    points
}

pub fn day9(lines: &Vec<String>) -> (i64, i64) {
    let grid = parse_grid(lines);
    let low_points = find_low_points(&grid);

    let part1 = low_points.iter().map(|v| i64::from(v.height) + 1).sum();

    (part1, 0)
}
