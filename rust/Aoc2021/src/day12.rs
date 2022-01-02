use std::collections::{HashMap, HashSet};

type NodeSet = HashSet<String>;
type Graph = HashMap<String, NodeSet>;

#[derive(Clone)]
struct Path {
    path: Vec<String>,
    has_double_small: bool,
}

fn build_graph(lines: &Vec<String>) -> Option<Graph> {
    let mut graph = Graph::new();

    for line in lines {
        let mut split = line.split('-');
        let from = split.next()?;
        let to = split.next()?;

        let entry = graph.entry(String::from(from)).or_insert(HashSet::new());
        entry.insert(String::from(to));

        let entry = graph.entry(String::from(to)).or_insert(HashSet::new());
        entry.insert(String::from(from));
    }

    Some(graph)
}

enum Part {
    Part1,
    Part2,
}

fn find_paths(node: &str, graph: &Graph, path: &Path, part: &Part) -> Vec<Path> {
    let mut paths = Vec::new();

    if node == "end" {
        paths.push(path.clone());
        return paths;
    }

    for neighbor in &graph[node] {
        if neighbor == "start" {
            continue;
        }

        let mut path = path.clone();

        let is_small = neighbor.chars().all(|c| c.is_lowercase());
        if is_small && path.path.contains(&neighbor) {
            match part {
                &Part::Part1 => continue,
                &Part::Part2 => {
                    if path.has_double_small {
                        continue;
                    } else {
                        path.has_double_small = true;
                    }
                },
            }
        }

        path.path.push(String::from(neighbor));

        for sub_path in find_paths(&neighbor, &graph, &path, part) {
            paths.push(sub_path);
        }
    }

    paths
}

fn count_paths(lines: &Vec<String>, part: &Part) -> i64 {
    let graph = build_graph(lines).unwrap();

    let start_path = Path {
        path: vec![String::from("start")],
        has_double_small: false,
    };

    let paths = find_paths("start", &graph, &start_path, part);

    paths.len().try_into().unwrap()
}

pub fn day12(lines: &Vec<String>) -> (i64, i64) {
    let part1 = count_paths(lines, &Part::Part1);
    let part2 = count_paths(lines, &Part::Part2);

    (part1, part2)
}
