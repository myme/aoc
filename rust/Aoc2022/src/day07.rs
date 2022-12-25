use std::collections::HashMap;

#[derive(Debug)]
struct Cursor {
    name: String,
    node: Tree,
    parent: Option<Box<Cursor>>,
}

type Tree = HashMap<String, Node>;

#[derive(Debug)]
enum Node {
    Tree(Tree),
    File(usize),
}

fn split_file_entry(input: &str) -> Option<(usize, &str)> {
    let mut parts = input.split(' ');
    let size = parts.next().and_then(|x| x.parse().ok())?;
    let name = parts.next()?;
    Some((size, name))
}

fn up(mut cursor: Cursor) -> Cursor {
    if let Cursor {
        name,
        parent: Some(parent),
        node,
    } = cursor
    {
        cursor = *parent;
        cursor.node.insert(name, Node::Tree(node));
    }

    cursor
}

fn build_tree(input: &str) -> Cursor {
    let mut cursor = Cursor {
        name: "/".to_string(),
        node: Tree::new(),
        parent: None,
    };

    for line in input.lines() {
        if let Some(path) = line.strip_prefix("$ cd ") {
            match path {
                "/" => {
                    if cursor.name != "/" {
                        panic!("Not at root!");
                    }
                }
                ".." => {
                    cursor = up(cursor);
                }
                _ => {
                    if let Some(Node::Tree(node)) = cursor.node.remove(path) {
                        cursor = Cursor {
                            name: path.to_string(),
                            node,
                            parent: Some(Box::new(cursor)),
                        };
                    } else {
                        panic!("No such directory: {}", path);
                    }
                }
            }
        } else if line == "$ ls" {
            // no-op
        } else if let Some(dir_name) = line.strip_prefix("dir ") {
            cursor
                .node
                .insert(dir_name.to_string(), Node::Tree(Tree::new()));
        } else if let Some((size, name)) = split_file_entry(line) {
            cursor.node.insert(name.to_string(), Node::File(size));
        } else {
            panic!("Invalid: {}", line);
        }
    }

    while cursor.parent.is_some() {
        cursor = up(cursor);
    }

    cursor
}

const MAX_SIZE: usize = 100000;

fn find_dir_size(tree: &Tree) -> (usize, Vec<usize>) {
    let mut size = 0;
    let mut sub_sizes = Vec::<usize>::new();

    for node in tree.values() {
        match node {
            Node::File(sz) => size += sz,
            Node::Tree(sub_tree) => {
                let (sz, mut sub_sz) = find_dir_size(sub_tree);
                size += sz;
                sub_sizes.append(&mut sub_sz);
                sub_sizes.push(sz);
            }
        };
    }

    (size, sub_sizes)
}

fn part1(sizes: &[usize]) -> String {
    let sum: usize = sizes.iter().filter(|x| **x < MAX_SIZE).sum();
    sum.to_string()
}

const DISK_SIZE: usize = 70_000_000;
const REQUIRED_SIZE: usize = 30_000_000;

fn part2(sizes: (usize, Vec<usize>)) -> String {
    let used_space = DISK_SIZE - sizes.0;
    let threshold = REQUIRED_SIZE - used_space;
    let min = sizes.1.iter().filter(|x| **x > threshold).min();
    min.unwrap_or(&0).to_string()
}

pub fn day7(input: &str) -> (String, String) {
    let tree = build_tree(input);
    let sizes = find_dir_size(&tree.node);
    (part1(&sizes.1), part2(sizes))
}
