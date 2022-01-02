use std::collections::{HashMap, HashSet};

type NodeSet = HashSet<String>;
type Graph = HashMap<String, NodeSet>;

#[derive(Clone)]
struct Path {
    nodes: NodeSet,
    has_revisited: bool,
}

impl Path {
    fn new() -> Path {
        Path {
            nodes: NodeSet::new(),
            has_revisited: false,
        }
    }

    fn contains(&self, part: &String) -> bool {
        self.nodes.contains(part)
    }

    fn add(&mut self, part: &str) {
        self.nodes.insert(String::from(part));
    }
}

fn build_graph(lines: &Vec<String>) -> Graph {
    let mut graph = Graph::new();

    for line in lines {
        let mut split = line.split('-');
        let from = split.next().unwrap();
        let to = split.next().unwrap();

        let entry = graph.entry(String::from(from)).or_insert(HashSet::new());
        entry.insert(String::from(to));

        let entry = graph.entry(String::from(to)).or_insert(HashSet::new());
        entry.insert(String::from(from));
    }

    graph
}

#[derive(Clone, Copy, PartialEq)]
enum Revisit { OnlyBig, OneSmall }

fn count_paths(node: &str, graph: &Graph, path: &mut Path, revisit: Revisit) -> i64 {
    if node == "end" {
        return 1;
    }

    let mut paths = 0;
    path.add(node);

    for neighbor in &graph[node] {
        if neighbor == "start" {
            continue;
        }

        let mut has_revisited = path.has_revisited;
        let is_small = neighbor.chars().all(|c| c.is_lowercase());
        if is_small && path.contains(&neighbor) {
            if revisit == Revisit::OnlyBig || has_revisited {
                continue;
            }
            has_revisited = true;
        }

        let mut path = path.clone();
        path.has_revisited = has_revisited;

        paths += count_paths(&neighbor, &graph, &mut path, revisit);
    }

    paths
}

pub fn day12(lines: &Vec<String>) -> (i64, i64) {
    let graph = build_graph(lines);

    let part1 = count_paths("start", &graph, &mut Path::new(), Revisit::OnlyBig);
    let part2 = count_paths("start", &graph, &mut Path::new(), Revisit::OneSmall);

    (part1, part2)
}
