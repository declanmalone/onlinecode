//! Alternative data structures for online code decoding
//!
//! Reimplementation of the BipartiteGraph structure using tables
//! (vectors), indices, etc. to avoid dynamicall allocating explicit
//! node and edge structures.


use std::collections::{HashSet,HashMap};



// "Equation" includes variables, but not in a way that is easy to
// search
struct Equation {
    count_unsolveds : usize,
    variables : HashSet<usize>,	// bundle of left nodes
    expanded : Vec<usize>,	// expansion on RHS
}

// Map variables onto equations in which they appear.
struct Variable {
    edges : HashSet<usize>,    
}
    
struct Decoder {
    mblocks : usize,		// message blocks
    ablocks : usize,		// auxiliary blocks
    coblocks: usize,		// mblocks + ablocks

    // equations stored in bipartite graph
    
    // thinking of calling these "variables" and "equations". We still
    // have the bipartite structure, but it's probably easier to
    // understand what's going on if we don't describe what's going on
    // in terms of graphs and edges ... I could even go further and
    // distinguish between "free" and "bound" as opposed to
    // "unsolved/unknown" and "solved"
    variables  : Vec<Variable>,	// left side is mblocks and ablocks
    equations : Vec<Equation>,	// right side is what they expand to

    // queue for breadth-first left-to-right scans. 
    bf_queue : Vec<usize>,
    
    count_unsolveds : usize,
    done : bool,
}

impl Decoder {

    fn new() {}

    // In order to avoid wrapping Option<Variable>, it makes sense to
    // pass in the auxiliary block mapping during construction. Every
    // block will then map to at least one equation, making the Option
    // unnecessary.

    // Need to sort the variables to allow sequential push
    fn add_aux_equations(&self,eq : &[usize]) {


    }

    // add check block to graph. Should we 
    fn add_check_eq(&self) {}

}

