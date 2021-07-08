//! # Bipartite Graph
//!
//! This module implements the bipartite graph as described in the
//! original paper by Maymounkov and Mazières.
//!
//! This implementation is not meant to be efficient. Instead, it is
//! meant to elucidate the bipartite graph data structure. Also, it's
//! an exercise for myself in working with graph structures in Rust.
//!
//! Nodes and Edges are dynamically allocated, with reference counting
//! provided by `Rc<T>` and, where needed, inner mutability provided
//! by `Rc<RefCell>` types.
//!
//! Node types are explicit. They can be one of [MessageNode],
//! [AuxiliaryNode] or [CheckNode].
//!
//! I follow the convention that message nodes are to the left of
//! auxiliary nodes, which are, in turn to the left of check nodes.
//!
//! As message and auxiliary blocks are solved, they are removed from
//! the graph entirely, although the BipartiteGraph struct maintains a
//! list of solved blocks separate from the actual graph structure.

// I'm going to implement a public trait that defines what an equation
// system does, then implement that trait here in terms of a bipartite
// graph.
//
// This implementation is not meant to be very efficient.
//
// I'll use Rc<T> to implement both nodes and edges.
//
// I'm giving each node its own type. This does increase the amount of
// repeated code, but it seems like a rusty way of doing things.

use std::collections::{HashSet, HashMap};
use std::rc::Rc;
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::ops::Deref;

/// MessageNode represents an unsolved message block
#[derive(Debug)]
pub struct MessageNode {
    id : u32,
    right_edges : HashSet<Rc<Edge>>,
}

impl MessageNode {
    fn new(id : u32) -> Self {
	let right_edges = HashSet::<Rc<Edge>>::new();
	Self { id, right_edges }
    }
    fn right_degree(&self) -> usize {
	self.right_edges.len()
    }
}

/// AuxiliaryNode represents an unsolved auxiliary block
#[derive(Debug)]
pub struct AuxiliaryNode {
    id : u32,
    left_edges : HashSet<Rc<Edge>>,
    right_edges : HashSet<Rc<Edge>>,
}

impl AuxiliaryNode {
    fn new(id : u32) -> Self {
	let left_edges  = HashSet::<Rc<Edge>>::new();
	let right_edges = HashSet::<Rc<Edge>>::new();
	Self { id, left_edges, right_edges }
    }
}

/// CheckNode represents a received check block that is comprised
/// of at least one unsolved message or auxiliary node
#[derive(Debug)]
pub struct CheckNode {
    id : u32,
    left_edges : HashSet<Rc<Edge>>,
}

impl CheckNode {
    fn new(id : u32) -> Self {
	let left_edges  = HashSet::<Rc<Edge>>::new();
	Self { id, left_edges }
    }
}

// I might also need a couple of extra types for left node and right
// node. Can I do it with enum? Or do I need a trait? 
//
// Actually, I can do either. 

/// An `Edge` can point to a `MessageNode` or `AuxiliaryNode` on the left.
#[derive(Clone, Debug)]
pub enum LeftNode {
    Msg(Rc<RefCell<MessageNode>>),
    Aux(Rc<RefCell<AuxiliaryNode>>),
}
/// An `Edge` can point to a `AuxiliaryNode` or `CheckNode` on the right.
#[derive(Clone, Debug)]
pub enum RightNode {
    Aux(Rc<RefCell<AuxiliaryNode>>),
    Chk(Rc<RefCell<CheckNode>>),
}

/// An `Edge` connects a left node and a right node
// #[derive(Eq)]
#[derive(Debug)]
pub struct Edge {
    id : u32,
    left_node : LeftNode,
    right_node : RightNode,
}

// use embedded id to support HashSet<Rc<Edge>>
impl Hash for Edge {
        fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialEq for Edge {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl Eq for Edge {}


/// Stores the state of unsolved message/auxiliary blocks in the
/// graph, along with other associated data (eg, solved blocks, state
/// of the solver)
pub struct BipartiteGraph {

    // don't even use vector for nodes: just a hash of id -> node
    msg_nodes : HashMap<u32, Rc<RefCell<MessageNode>>>,
    aux_nodes : HashMap<u32, Rc<RefCell<AuxiliaryNode>>>,
    chk_nodes : HashMap<u32, Rc<RefCell<CheckNode>>>,

    // won't store edges here; references to them are stored in nodes

    // will have auto-incrementing node IDs, though. Let's keep track
    // of each one separately for now.
    next_msg_id : u32,
    next_aux_id : u32,
    next_chk_id : u32,
    next_edj_id : u32,
}

impl BipartiteGraph {

    /// Constructor
    pub fn new() -> Self {
	Self {
	    msg_nodes : HashMap::<u32, Rc<RefCell<MessageNode>>>::new(),
	    aux_nodes : HashMap::<u32, Rc<RefCell<AuxiliaryNode>>>::new(),
	    chk_nodes : HashMap::<u32, Rc<RefCell<CheckNode>>>::new(),
	    next_msg_id : 0,
	    next_aux_id : 0,
	    next_chk_id : 0,
	    next_edj_id : 0,
	}
    }

    fn with_size(msg : u32, aux : u32, chk : u32, edges : u32) {
	panic!("TODO: implement storing nodes in vectors");
	// by implementing this, we don't need to create nodes
	// manually. Also, the need for dynamic allocation goes away
	// completely.
	//
	// However, that kind of defeats the purpose of what I want to
	// implement first: a purely dynamic version of the graph
	// using Rc<Foo> throughout.
    }
    
    /// Create a new message node
    // Caller will have to call this once for each message node
    pub fn new_msg_node(&mut self) -> u32 {
	let this_id = self.next_msg_id;
	self.msg_nodes.
	    insert(this_id,
		   Rc::new(RefCell::new(MessageNode::new(this_id))));
	self.next_msg_id += 1;
	this_id
    }

    /// Create a new auxiliary node
    pub fn new_aux_node(&mut self) -> u32 {
	let this_id = self.next_aux_id;
	self.aux_nodes
	    .insert(this_id,
		    Rc::new(RefCell::new(AuxiliaryNode::new(this_id))));
	self.next_aux_id += 1;
	this_id
    }

    /// Create a new check node
    pub fn new_chk_node(&mut self) -> u32 {
	let this_id = self.next_chk_id;
	self.chk_nodes
	    .insert(this_id,
		    Rc::new(RefCell::new(CheckNode::new(this_id))));
	self.next_chk_id += 1;
	this_id
    }

    // Can't have a polymorphic new_edge fn...
    /// Does nothing. Use constructors that connect specific node types
    fn new_edge() {

    }

    // So make explicitly-typed variants
    /// Create a new edge connecting a message node and an auxiliary node
    pub fn new_msg_aux_edge(&mut self, msg_id : u32, aux_id : u32) {
	let msg = self.msg_nodes.get_mut(&msg_id).unwrap();
	let aux = self.aux_nodes.get_mut(&aux_id).unwrap();

	// let left_node  = Rc::clone(&msg);
	// let right_node = Rc::clone(&aux);

	let this_edge_id = self.next_edj_id;
	self.next_edj_id += 1;

	// have to wrap in an Rc and enum
	let edge = Rc::new(Edge {
	    id : this_edge_id,
	    left_node  : LeftNode::Msg(Rc::clone(&msg)),
	    right_node : RightNode::Aux(Rc::clone(&aux))
	});

	// Now clone the Rc<Edge>, storing them in the two nodes'
	// HashMaps

	// fails because we don't have Hash, Eq for Rc<Edge> ...
	//
	// I tried deriving them with #[derive(...)] but it just threw
	// up more problems...

	// Can change HashSet<Rc<Edge>> to HashMap<u32, Rc<Edge>>
	// and put an id inside each edge like I did for nodes
	//
	// Or, I can have a HashSet<u32> containing edge IDs and
	// store the actual edges in Self.
	//
	// Actually, instead of using #[derive], I can implement Hash
	// for Edge manually. See:
	//
	// https://users.rust-lang.org/t/what-do-i-need-to-do-to-hash-a-hashset-rc-my/27093/17

	// next problem ... inner mutability? Rc<Cell<>>? RefRefCell?
	//
	// Actually:
	//
	// https://manishearth.github.io/blog/2015/05/27/wrapper-types-in-rust-choosing-your-guarantees/
	//
	// Could I use & instead? I think not ... I'll try Cell first,
	// and if that doesn't work, I'll use RefRefCell, which is only
	// checked at runtime.

	// As an alternative to the below, we could use
	// accessors. They would still have to do the same mutable
	// borrow, but we wouldn't have to repeat ourselves so much.
	//
	// Could be called anchor_left_end(), anchor_right_end()
	
	msg.borrow_mut().right_edges.insert(Rc::clone(&edge));
	aux.borrow_mut().left_edges.insert(Rc::clone(&edge));
    }

    /// Create a new edge connecting a message node and a check node
    pub fn new_msg_chk_edge(&mut self, msg_id : u32, chk_id : u32) {
	let msg = self.msg_nodes.get_mut(&msg_id).unwrap();
	let chk = self.chk_nodes.get_mut(&chk_id).unwrap();

	let this_edge_id = self.next_edj_id;
	self.next_edj_id += 1;

	// have to wrap in an Rc and enum
	let edge = Rc::new(Edge {
	    id : this_edge_id,
	    left_node  : LeftNode::Msg(Rc::clone(&msg)),
	    right_node : RightNode::Chk(Rc::clone(&chk))
	});

	msg.borrow_mut().right_edges.insert(Rc::clone(&edge));
	chk.borrow_mut().left_edges.insert(Rc::clone(&edge));
    }

    /// Create a new edge connecting an auxiliary node and a check node
    pub fn new_aux_chk_edge(&mut self, aux_id : u32, chk_id : u32) {
	let aux = self.aux_nodes.get_mut(&aux_id).unwrap();
	let chk = self.chk_nodes.get_mut(&chk_id).unwrap();

	let this_edge_id = self.next_edj_id;
	self.next_edj_id += 1;

	// have to wrap in an Rc and enum
	let edge = Rc::new(Edge {
	    id : this_edge_id,
	    left_node  : LeftNode::Aux(Rc::clone(&aux)),
	    right_node : RightNode::Chk(Rc::clone(&chk))
	});

	aux.borrow_mut().right_edges.insert(Rc::clone(&edge));
	chk.borrow_mut().left_edges.insert(Rc::clone(&edge));
    }

    // References to the edge are stored in nodes, so if we drop
    // those, the edge's refcount should go to zero. Then, the
    // references that the edge itself holds (to the nodes) should
    // be automatically dropped too.
    //
    // Where will this be called from?
    //
    // We don't have a table of edges in BipartiteGraph, so we can't
    // look up a given edge ID. Instead, we'll find an edge while
    // we're examining nodes. Deleting it will involve removing both
    // links to the edge. 
    //
    // 

    pub fn delete_edge(&mut self, edge : &Rc<Edge>)
		       -> (LeftNode, RightNode) {
	// return these as a convenience to caller
	let (left,right) = (edge.left_node.clone(), edge.right_node.clone());
//	let edge_id = edge.id;

	// Compiler suggested adding ref (seems good to me)
	match left {
	    LeftNode::Msg(ref msg) => {
		assert_eq!(true,
			   msg.borrow_mut().right_edges.remove(edge)) },
	    LeftNode::Aux(ref aux) => {
		assert_eq!(true,
			   aux.borrow_mut().right_edges.remove(edge)) },
	};

	match right {
	    RightNode::Aux(ref aux) => {
		assert_eq!(true, aux.borrow_mut().left_edges.remove(edge)) },
	    RightNode::Chk(ref chk) => {
		assert_eq!(true, chk.borrow_mut().left_edges.remove(edge)) },
	};

	(left, right)
    }
    
}

#[cfg(test)]

mod tests {

    use super::*;

    #[test]
    fn test_make_new_bipartite() {
	let _graph = BipartiteGraph::new();
    }

    // The following test is quite long and tests quite a few things.
    // However, it's hard to break it up into individual tests since
    // each step builds on the previous ones.
    #[test]
    fn test_link_msg_aux_nodes() {
	let mut graph = BipartiteGraph::new();
	let msg_id = graph.new_msg_node();
	let aux_id = graph.new_aux_node();

	// IDs have their own separate counters
	assert_eq!(msg_id, aux_id);

	// make the edge
	graph.new_msg_aux_edge(msg_id,aux_id);

	// confirm that links to the edge exist in msg, aux nodes
	assert_eq!(1, graph.msg_nodes.len());
	assert_eq!(1, graph.aux_nodes.len());
	assert_eq!(0, graph.chk_nodes.len());

	// pull out the nodes
	let msg = graph.msg_nodes.get(&msg_id).unwrap();
	let aux = graph.aux_nodes.get(&aux_id).unwrap();

	// borrow msg, aux into temporary (shadowed) variables
	let msg = msg.borrow();
	let aux = aux.borrow();

	// confirm that they have right, left edges in the right
	// places
	assert_eq!(1, msg.right_edges.len());
	assert_eq!(0, aux.right_edges.len());
	assert_eq!(1, aux.left_edges.len());

	// confirm that left side, right side point to the same edge
	let left_side = msg.right_edges.iter().next().unwrap();
	let right_side = aux.left_edges.iter().next().unwrap();
	assert_eq!(left_side, right_side);

	// could also let edge = right_side (same edge)
	let edge = left_side;
	
	// confirm that the edge thinks it is connected to our nodes
	// msg (on left) <--- edge ---> aux (on right)
	
	// destructure the enums that are stored in the edge in order
	// to compare on IDs rather than figuring out the correct type
	// coercions needed to compare references with each other...

	// is our msg node on the left?
	let left_id = match edge.left_node {
	    LeftNode::Msg(ref msg) => { msg.borrow().id },
	    LeftNode::Aux(ref aux) => { aux.borrow().id },
	};
	assert_eq!(msg.id, left_id);

	// is our aux node on the right?
	let right_id = match edge.right_node {
	    RightNode::Aux(ref aux) => { aux.borrow().id },
	    RightNode::Chk(ref chk) => { chk.borrow().id },
	};
	assert_eq!(aux.id, right_id);

	// relinquish our immutable borrows on graph
	let edge = edge.clone(); // had a ref to node of graph
	drop(msg);
	drop(aux);

	// delete the edge (requires graph be mutably borrowed)
	let (leftnode,rightnode) = graph.delete_edge(&edge);

	// these left, right nodes are wrapped in enums, so if we want
	// to check their ids we have to destructure them again

	let removed_left_id = match edge.left_node {
	    LeftNode::Msg(ref msg) => { msg.borrow().id },
	    LeftNode::Aux(ref aux) => { aux.borrow().id },
	};
	let removed_right_id = match edge.right_node {
	    RightNode::Aux(ref aux) => { aux.borrow().id },
	    RightNode::Chk(ref chk) => { chk.borrow().id },
	};
	// we already saved left_id, right_id variables
	assert_eq!(removed_left_id, left_id);
	assert_eq!(removed_right_id, right_id);
    }
}
