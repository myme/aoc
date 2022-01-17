use std::collections::{HashSet, VecDeque};

use crate::utils;

struct Grid {
    rows: Vec<Vec<u8>>,
    width: usize,
    height: usize,
}

fn parse_grid(lines: &Vec<String>) -> Grid {
    let mut rows: Vec<Vec<u8>> = Vec::new();

    for line in lines {
        let row: Vec<u8> = line.chars().map(|v| (v as u8) - 0x30).collect();
        rows.push(row);
    }

    let height = rows.len();
    let width = rows[0].len();

    Grid {
        rows,
        width,
        height,
    }
}

struct Point {
    row: usize,
    col: usize,
    height: u8,
}

fn find_low_points(grid: &Grid) -> Vec<Point> {
    let mut points = Vec::new();

    for (row_idx, row) in grid.rows.iter().enumerate() {
        for (col_idx, height) in row.iter().enumerate() {
            if row_idx > 0 && height >= &grid.rows[row_idx - 1][col_idx] {
                continue;
            }
            if col_idx > 0 && height >= &grid.rows[row_idx][col_idx - 1] {
                continue;
            }
            if row_idx < grid.height - 1 && height >= &grid.rows[row_idx + 1][col_idx] {
                continue;
            }
            if col_idx < grid.width - 1 && height >= &grid.rows[row_idx][col_idx + 1] {
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

type Basins = Vec<HashSet<(usize, usize)>>;

fn find_basins(grid: &Grid, points: &Vec<Point>) -> Basins {
    let mut basins = Vec::new();

    for Point { row, col, .. } in points {
        let mut basin: HashSet<(usize, usize)> = HashSet::new();
        let mut candidates = VecDeque::from([(*row, *col)]);

        while let Some((row, col)) = candidates.pop_front() {
            if basin.contains(&(row, col)) {
                continue;
            }

            let height = grid.rows[row][col];
            if height == 9 {
                continue;
            }

            basin.insert((row, col));

            if row > 0 && height <= grid.rows[row - 1][col] {
                candidates.push_back((row - 1, col));
            }
            if col > 0 && height <= grid.rows[row][col - 1] {
                candidates.push_back((row, col - 1));
            }
            if row < grid.height - 1 && height <= grid.rows[row + 1][col] {
                candidates.push_back((row + 1, col));
            }
            if col < grid.width - 1 && height <= grid.rows[row][col + 1] {
                candidates.push_back((row, col + 1));
            }
        }

        basins.push(basin);
    }

    basins
}

pub fn day9(input: &str) -> (i64, i64) {
    let grid = parse_grid(&utils::to_lines(input));
    let low_points = find_low_points(&grid);

    let part1 = low_points.iter().map(|v| i64::from(v.height) + 1).sum();

    let mut basins: Vec<i64> = find_basins(&grid, &low_points)
        .iter()
        .map(|b| b.len() as i64)
        .collect();
    basins.sort();
    basins.reverse();
    let part2 = basins.iter().take(3).product();

    (part1, part2)
}
