pub mod adj;
pub mod map;

use std::{
    collections::{HashMap, VecDeque},
    hash::Hash,
};

pub use adj::AdjGraph;
pub use map::MapGraph;

pub trait Graph<T> {
    type NodeIdx<'a>: Copy + PartialEq + Eq + Hash
    where
        T: 'a;
    type Weight;

    type This<U, W>;

    fn new_empty() -> Self;

    fn size(&self) -> usize;

    fn nodes<'a>(&'a self) -> impl Iterator<Item = Self::NodeIdx<'a>>
    where
        T: 'a;
    fn neighbours<'a>(&'a self, from: Self::NodeIdx<'a>) -> impl Iterator<Item = Self::NodeIdx<'a>>
    where
        T: 'a;
    fn key<'a>(&'a self, node: Self::NodeIdx<'a>) -> &'a T
    where
        T: 'a;

    fn insert<'a>(&'a mut self, key: T) -> Self::NodeIdx<'a>
    where
        T: 'a;
    fn link<'a>(&mut self, from: Self::NodeIdx<'a>, to: Self::NodeIdx<'a>)
    where
        T: 'a;

    fn set_weight<'a>(&'a mut self, val: Self::NodeIdx<'a>, weight: Self::Weight);
    fn get_weight<'a>(&self, val: Self::NodeIdx<'a>) -> Option<&Self::Weight>
    where
        T: 'a;

    fn insert_weight<'a>(&'a mut self, key: T, weight: Self::Weight) -> Self::NodeIdx<'a>
    where
        T: 'a;

    /// Returns a graph with an identical shape, but using the indices instead. The indices are the
    /// ones from `nodes`, not `neighbours` for implementations in which the distinction matters
    fn index_graph(&self) -> <Self as Graph<T>>::This<Self::NodeIdx<'_>, &Self::Weight>;

    /// f must be a morphism, meaning that the implemetation of PartialEq and Hash for x, y of type
    /// T must agree with the implementation of PartialEq and Hash of f(x) and f(y).
    ///
    /// In particular this means that f must be pure, as it should return the same output for the
    /// same input
    fn map<U, F>(self, f: F) -> <Self as Graph<T>>::This<U, Self::Weight>
    where
        F: FnMut(T) -> U,
        U: PartialEq + Eq + Hash;
}

pub trait GraphClone<T>: Graph<T, Weight: Clone> {
    /// Clones all nodes but not edges. Guarantees that the cloned NodeIdx can access both graphs
    fn node_clone(&self) -> Self;
}

pub fn reversed<T, G>(graph: &G) -> G
where
    G: GraphClone<T>,
    T: Clone,
{
    let mut reverse = graph.node_clone();

    for node in graph.nodes() {
        for neighbour in graph.neighbours(node) {
            reverse.link(neighbour, node);
        }
    }

    reverse
}

pub fn topological<'a, T, G>(graph: &'a G) -> Vec<(G::NodeIdx<'a>, Option<&'a G::Weight>)>
where
    G: Graph<T>,
{
    #[derive(Clone, Copy, PartialEq, Eq)]
    enum Mark {
        None,
        Temp,
        Perm,
    }

    fn visit<'a, T, G: Graph<T>>(
        node: G::NodeIdx<'a>,
        graph: &'a G,
        visited: &mut HashMap<G::NodeIdx<'a>, Mark>,
        sorted: &mut VecDeque<(G::NodeIdx<'a>, Option<&'a G::Weight>)>,
    ) {
        let mark = visited.get_mut(&node).expect("Node was absent from graph");

        match *mark {
            Mark::None => (),
            Mark::Temp => panic!("Cycle detected in graph"),
            Mark::Perm => return,
        }

        *mark = Mark::Temp;

        for neighbour in graph.neighbours(node) {
            visit(neighbour, graph, visited, sorted);
        }

        *visited.get_mut(&node).expect("Node was removed from graph") = Mark::Perm;
        sorted.push_front((node, graph.get_weight(node)));
    }

    let mut visited: HashMap<_, _> = graph.nodes().map(|n| (n, Mark::None)).collect();
    let mut sorted = VecDeque::new();

    while visited.values().any(|&v: &Mark| v != Mark::Perm) {
        let &unmarked = visited
            .iter()
            .find(|&(_, &m)| m == Mark::None)
            .expect("No unmarked node found")
            .0;

        visit(unmarked, graph, &mut visited, &mut sorted);
    }

    sorted.into()
}
