//! # Compatibility trait between BipartiteGraph and Equations
//!

use rand::{Rng,thread_rng};
use crate::floyd::*;
use crate::rng;
use std::collections::HashSet;

// Make a structure to encapsulate auxiliary block mapping
//
// Each message block attaches to q auxiliary blocks
//

pub struct AuxMapping {
    aux_to_mblocks : Vec<Vec<usize>>
}

impl AuxMapping {
    // q : Each message block attaches to q auxiliary blocks
    fn new(rng : &mut impl Rng, mblocks : usize, ablocks : usize, q : usize)
    -> Self {
	// outer vector
	let mut aux_to_mblocks = Vec::with_capacity(ablocks);
	// inner vectors, initially empty
	for _ in 0..ablocks {
	    aux_to_mblocks.push(Vec::<usize>::new())
	}
	for mblock in 0..mblocks {
	    // uniformly select q aux blocks for this message block
	    let hashset = floyd_usize(rng, q, ablocks);

	    for ablock in hashset.iter() {
		aux_to_mblocks[*ablock as usize].push(mblock)
	    }
	}
	AuxMapping { aux_to_mblocks }
    }
}

#[cfg(test)]
mod test_auxmapping {
    use super::*;
    use rand::{Rng,thread_rng};

    #[test]
    fn exactly_q_aux_blocks() {
	let mut rng = thread_rng();
	let mapping = AuxMapping::new(&mut rng, 20, 5, 5);
	for aux in mapping.aux_to_mblocks.iter() {
	    // each aux block should be mapped to each message block
	    assert_eq!(aux.len(), 20);
	    // no need to check if they're distinct because these
	    // values came from a hashmap...
	}
    }

    #[test]
    #[should_panic]
    fn q_too_high() {
	let mut rng = thread_rng();
	// pick 6 aux blocks out of 5 choices: no good!
	let mapping = AuxMapping::new(&mut rng, 20, 5, 6);
    }

}

// Things that both will have to implement
trait Compat {

    // constructor...
    fn new(mblocks : usize, ablocks : usize) -> Self;

    // adding auxiliary block mapping

    // take auxiliary block mapping and add all edges to graph
    fn add_auxiliary_map(&self, map : &AuxMapping);

    // return status: count of remaining unsolved blocks
    fn unsolved_msg_blocks(&self) -> usize;

    // Make some executive decisions here ...
    //
    // * caller manages check blocks, and only passes in the ID
    // * as a result, xor'ing blocks is the caller's responsibility
    // * to distinguish msg, aux blocks, start numbering aux after msg

    // I would like to implement single-stepping to help with
    // debugging. That is: the solver returns as soon as it has solved
    // one node, but keeps an internal list of nodes that might have
    // become solvable thanks to that solution.
    //
    // However, 
    
    fn add_check_block(&self, check_id : usize, blocks : Vec<usize>);

    
}
