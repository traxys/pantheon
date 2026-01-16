use std::{borrow::Borrow, collections::HashMap, hash::Hash, ops::Deref};

use crate::{Graph, GraphClone};

// MapGraph guarantees that while it clones the weights supplied it does not hand a reference from
// a cloned value
#[derive(Debug)]
#[repr(transparent)]
pub struct MapRef<'a, T>(&'a T);

impl<'a, T> Clone for MapRef<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, T> PartialEq for MapRef<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0, other.0)
    }
}

impl<'a, T> Eq for MapRef<'a, T> {}

impl<'a, T> Hash for MapRef<'a, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.0, state)
    }
}

impl<'a, T> Copy for MapRef<'a, T> {}

impl<'a, T> Deref for MapRef<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

#[derive(Debug)]
pub struct MapGraph<T, V> {
    nodes: HashMap<T, (Option<V>, Vec<T>)>,
}

impl<T: PartialEq + Eq + Hash, V> MapGraph<T, V> {
    pub fn contains<Q>(&self, value: &Q) -> bool
    where
        T: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.nodes.contains_key(value)
    }

    pub fn get_ref<Q>(&self, value: &Q) -> MapRef<'_, T>
    where
        T: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        MapRef(
            self.nodes
                .get_key_value(value)
                .expect("Value was not present in map")
                .0,
        )
    }

    pub fn set_weight_ref<Q>(&mut self, value: &Q, weight: V)
    where
        T: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.nodes.get_mut(value).unwrap().0 = Some(weight)
    }
}

impl<T: PartialEq + Eq + Hash + Clone, V> MapGraph<T, V> {
    pub fn link_ref<S, D>(&mut self, from: &S, to: &D)
    where
        D: ToOwned<Owned = T> + Eq + Hash + ?Sized,
        S: Eq + Hash + ?Sized,
        T: Borrow<S> + Borrow<D>,
    {
        if !self.nodes.contains_key(from) || !self.nodes.contains_key(to) {
            panic!("Attempted to link unrelated values");
        }

        let neighbours = &mut self.nodes.get_mut(from).unwrap().1;
        if !neighbours.iter().any(|v| <T as Borrow<D>>::borrow(v) == to) {
            neighbours.push(to.to_owned());
        }
    }

    fn insert_raw_weight<'a>(&'a mut self, key: T, weight: Option<V>) -> MapRef<'a, T> {
        if self
            .nodes
            .insert(key.clone(), (weight, Vec::new()))
            .is_some()
        {
            panic!("Value was already present")
        }

        self.get_ref(&key)
    }
}

/// The Clone implementation must maintain Hash and Eq
impl<T, V> Graph<T> for MapGraph<T, V>
where
    T: Clone + PartialEq + Eq + Hash,
    V: Clone,
{
    type NodeIdx<'a>
        = MapRef<'a, T>
    where
        T: 'a;
    type Weight = V;

    type This<U, W> = MapGraph<U, W>;

    fn new_empty() -> Self {
        Self {
            nodes: HashMap::new(),
        }
    }

    fn size(&self) -> usize {
        self.nodes.len()
    }

    fn nodes<'a>(&'a self) -> impl Iterator<Item = Self::NodeIdx<'a>>
    where
        T: 'a,
    {
        self.nodes.keys().map(MapRef)
    }

    fn neighbours<'a>(&'a self, from: Self::NodeIdx<'a>) -> impl Iterator<Item = Self::NodeIdx<'a>>
    where
        T: 'a,
    {
        self.nodes[from.0].1.iter().map(|n| self.get_ref(n))
    }

    fn key<'a>(&self, node: Self::NodeIdx<'a>) -> &'a T
    where
        T: 'a,
    {
        node.0
    }

    fn insert<'a>(&'a mut self, key: T) -> Self::NodeIdx<'a>
    where
        T: 'a,
    {
        self.insert_raw_weight(key, None)
    }

    fn insert_weight<'a>(&'a mut self, key: T, weight: Self::Weight) -> Self::NodeIdx<'a>
    where
        T: 'a,
    {
        self.insert_raw_weight(key, Some(weight))
    }

    fn set_weight<'a>(&'a mut self, val: Self::NodeIdx<'a>, weight: Self::Weight) {
        self.set_weight_ref(val.0, weight);
    }

    fn get_weight<'a>(&self, val: Self::NodeIdx<'a>) -> Option<&Self::Weight>
    where
        T: 'a,
    {
        self.nodes[val.0].0.as_ref()
    }

    fn link<'a>(&mut self, from: Self::NodeIdx<'a>, to: Self::NodeIdx<'a>)
    where
        T: 'a,
    {
        self.link_ref(from.0, to.0);
    }

    fn index_graph(&self) -> <Self as Graph<T>>::This<Self::NodeIdx<'_>, &V> {
        MapGraph {
            nodes: self
                .nodes
                .iter()
                .map(|(k, ns)| {
                    (
                        MapRef(k),
                        (
                            ns.0.as_ref(),
                            ns.1.iter().map(|n| self.get_ref(n)).collect(),
                        ),
                    )
                })
                .collect(),
        }
    }

    fn map<U, F>(self, mut f: F) -> <Self as Graph<T>>::This<U, Self::Weight>
    where
        F: FnMut(T) -> U,
        U: PartialEq + Eq + Hash,
    {
        MapGraph {
            nodes: self
                .nodes
                .into_iter()
                .map(|(k, v)| (f(k), (v.0, v.1.into_iter().map(&mut f).collect())))
                .collect(),
        }
    }
}

impl<T, V> GraphClone<T> for MapGraph<T, V>
where
    T: Clone + PartialEq + Eq + Hash,
    V: Clone,
{
    fn node_clone(&self) -> Self {
        Self {
            nodes: self
                .nodes
                .iter()
                .map(|(k, (w, _))| (k.clone(), (w.clone(), Vec::new())))
                .collect(),
        }
    }
}
