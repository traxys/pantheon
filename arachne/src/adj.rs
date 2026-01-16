use crate::{Graph, GraphClone};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeIndex(u32);

#[derive(Debug)]
pub struct AdjGraph<T> {
    nodes: Vec<(T, Vec<u32>)>,
}

impl<T> AdjGraph<T> {
    pub fn new() -> Self {
        Self { nodes: Vec::new() }
    }
}

impl<T> Default for AdjGraph<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Graph<T> for AdjGraph<T> {
    type NodeIdx<'a>
        = NodeIndex
    where
        T: 'a;
    type Weight = ();

    type This<U, W> = AdjGraph<U>;

    fn size(&self) -> usize {
        self.nodes.len()
    }

    fn nodes<'a>(&self) -> impl Iterator<Item = Self::NodeIdx<'a>>
    where
        T: 'a,
    {
        (0..self.nodes.len()).map(|v| NodeIndex(v as u32))
    }

    fn neighbours<'a>(&self, from: Self::NodeIdx<'a>) -> impl Iterator<Item = Self::NodeIdx<'a>>
    where
        T: 'a,
    {
        self.nodes[from.0 as usize].1.iter().map(|&v| NodeIndex(v))
    }

    fn new_empty() -> Self {
        Self::new()
    }

    fn key<'a>(&'a self, node: Self::NodeIdx<'a>) -> &'a T
    where
        T: 'a,
    {
        &self.nodes[node.0 as usize].0
    }

    fn insert<'a>(&mut self, weight: T) -> Self::NodeIdx<'a>
    where
        T: 'a,
    {
        let index = self.nodes.len();
        self.nodes.push((weight, Vec::new()));
        NodeIndex(index as u32)
    }

    fn insert_weight<'a>(&'a mut self, key: T, (): Self::Weight) -> Self::NodeIdx<'a>
    where
        T: 'a,
    {
        self.insert(key)
    }

    fn set_weight<'a>(&'a mut self, _: Self::NodeIdx<'a>, (): Self::Weight) {}
    fn get_weight<'a>(&self, _: Self::NodeIdx<'a>) -> Option<&Self::Weight>
    where
        T: 'a,
    {
        None
    }

    fn link<'a>(&mut self, from: Self::NodeIdx<'a>, to: Self::NodeIdx<'a>)
    where
        T: 'a,
    {
        let (_, edges) = self
            .nodes
            .get_mut(from.0 as usize)
            .expect("Node from was not found");

        if !edges.contains(&to.0) {
            edges.push(to.0);
        }
    }

    fn index_graph(&self) -> <Self as Graph<T>>::This<Self::NodeIdx<'_>, &Self::Weight> {
        AdjGraph {
            nodes: self
                .nodes
                .iter()
                .enumerate()
                .map(|(i, (_, ns))| (NodeIndex(i as u32), ns.clone()))
                .collect(),
        }
    }

    fn map<U, F>(self, mut f: F) -> <Self as Graph<T>>::This<U, Self::Weight>
    where
        F: FnMut(T) -> U,
        U: PartialEq + Eq + std::hash::Hash,
    {
        AdjGraph {
            nodes: self.nodes.into_iter().map(|(w, n)| (f(w), n)).collect(),
        }
    }
}

impl<T: Clone> GraphClone<T> for AdjGraph<T> {
    fn node_clone(&self) -> Self {
        Self {
            nodes: self
                .nodes
                .iter()
                .map(|(v, _)| (v.clone(), Vec::new()))
                .collect(),
        }
    }
}
