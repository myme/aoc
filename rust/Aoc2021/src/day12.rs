use std::collections::{HashMap, HashSet};

type NodeSet = HashSet<String>;
type Graph = HashMap<String, NodeSet>;

#[derive(Clone)]
struct Path {
    nodes: NodeSet,
    has_double_small: bool,
}

impl Path {
    fn new() -> Path {
        Path {
            nodes: NodeSet::new(),
            has_double_small: false,
        }
    }

    fn contains(&self, part: &String) -> bool {
        self.nodes.contains(part)
    }

    fn add(&mut self, part: &str) {
        self.nodes.insert(String::from(part));
    }
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

fn count_paths_helper(node: &str, graph: &Graph, path: &mut Path, part: &Part) -> i64 {
    if node == "end" {
        return 1;
    }

    let mut paths = 0;
    path.add(node);

    for neighbor in &graph[node] {
        if neighbor == "start" {
            continue;
        }

        let mut has_double_small = path.has_double_small;
        let is_small = neighbor.chars().all(|c| c.is_lowercase());
        if is_small && path.contains(&neighbor) {
            match part {
                &Part::Part1 => continue,
                &Part::Part2 => {
                    if has_double_small {
                        continue;
                    } else {
                        has_double_small = true;
                    }
                },
            }
        }

        let mut path = path.clone();
        path.has_double_small = has_double_small;

        paths += count_paths_helper(&neighbor, &graph, &mut path, part);
    }

    paths
}

fn count_paths(lines: &Vec<String>, part: &Part) -> i64 {
    let graph = build_graph(lines).unwrap();

    count_paths_helper("start", &graph, &mut Path::new(), part)
}

pub fn day12(lines: &Vec<String>) -> (i64, i64) {
    let part1 = count_paths(lines, &Part::Part1);
    let part2 = count_paths(lines, &Part::Part2);

    (part1, part2)
}
