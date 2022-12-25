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

fn split_file_entry<'a>(input: &'a str) -> Option<(usize, &'a str)> {
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
        cursor.node.insert(name.to_string(), Node::Tree(node));
    }

    cursor
}

fn build_tree(input: &str) -> Cursor {
    let mut lines = input.lines();
    let mut cursor = Cursor {
        name: "/".to_string(),
        node: Tree::new(),
        parent: None,
    };

    while let Some(line) = lines.next() {
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

fn part1(tree: &Cursor) -> String {
    let (_, sub_sizes) = find_dir_size(&tree.node);
    let sum: usize = sub_sizes.iter().filter(|x| **x < MAX_SIZE).sum();
    sum.to_string()
}

pub fn day7(input: &str) -> (String, String) {
    let tree = build_tree(input);
    (part1(&tree), String::new())
}
