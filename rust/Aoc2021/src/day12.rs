use std::collections::{HashMap, HashSet};

type NodeSet = HashSet<String>;
type Graph = HashMap<String, NodeSet>;
type Path = Vec<String>;

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

fn find_paths(node: &str, graph: &Graph, path: &Path) -> Vec<Path> {
    let mut paths = Vec::new();

    if node == "end" {
        paths.push(path.clone());
        return paths;
    }

    for neighbor in &graph[node] {
        let is_small = neighbor.chars().all(|c| c.is_lowercase());
        if is_small && path.contains(&neighbor) {
            continue;
        }

        let mut path = path.clone();
        path.push(String::from(neighbor));

        for sub_path in find_paths(&neighbor, &graph, &path) {
            paths.push(sub_path);
        }
    }

    paths
}

fn count_paths(lines: &Vec<String>) -> i64 {
    let graph = build_graph(lines).unwrap();
    let paths = find_paths("start", &graph, &vec![String::from("start")]);
    paths.len().try_into().unwrap()
}

pub fn day12(lines: &Vec<String>) -> (i64, i64) {
    let part1 = count_paths(lines);

    (part1, 0)
}
