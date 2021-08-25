//! Alternative data structures for online code decoding
//!
//! Reimplementation of the BipartiteGraph structure using tables
//! (vectors), indices, etc. to avoid dynamically allocating explicit
//! node and edge structures.


use std::collections::{HashSet,HashMap};

use crate::compat::*;

// There's a degree of duplication in the structures below.
//
// An Equation represents a check block, eg:
//
// Msg1 + Msg6 + Aux3 = data_block_4
//
// Upon receipt, we check whether any of the variables on the left
// side are known, and if so, we substitute them in. Then:
//
// case 1: all variables are known
//
// In this case, the equation provides no new information, so it is
// discarded.
//
// case 2: all but one variables are known
//
// substitute in the knowns by subtracting them from both sides, eg:
//
// Msg6 = data_block_4 - Msg1 - Aux3
//
// This solves a new variable. In this case, we would mark Msg6 as
// solved and queue up that variable for checking to see if makes any
// other equations solvable.
//
// default:
//
// Substitute in any knowns and insert equation into the system
//

type EqID = usize;
type VarID = usize;

pub struct Equation {
    // As more variables become solved (case 2 above), they can be
    // substituted into other equations, so values (variable ids) will
    // move from the HashSet on the left to the Vec on the right.
    // Eventually, there will only be a single variable on the left
    pub lhs : HashSet<VarID>,       // unknown variables
    pub rhs : Vec<VarID>,           // expansion (data block [+vars])
    // Note: we don't store block IDs explicitly, but it is part of
    // the expansion on the right. The block ID can be inferred by the
    // index of the equation in the table.

    // count_unsolveds is not strictly necessary since we can simply
    // count how many items are in the lhs HashSet ...
    // count_unsolveds : usize,
}

// We also need to map variables onto the equations in which they
// appear.
pub struct Variable {
    pub equations : HashSet<EqID>,
}

// Further note on variable, equation and block IDs... 
//
// Note that before we can accept check blocks, we have to set up
// equations for the auxiliary mappings. Since they're explicitly set
// up at the start, we have to reserve `ablocks` entries in the
// equations array.
//
// Also, the equations in the auxiliary mapping are slightly different
// from equations derived from check blocks...
//
// A check block equation:
//
// Msg1 + Msg6 + Aux3 = data_block_4
//
// An auxiliary block equation:
//
// Msg1 + Msg4 + Msg7 + ... = Aux1
//
// We can rewrite this by bringing Aux1 to the left, since at the
// start of decoding, its value is unknown.
//
// Msg1 + Msg4 + Msg7 + ... + Aux1 = 0
//
// This zero on the right can be represented as a virtual BlockID 0.
//
// If we're not explicitly storing a block ID within an Equation, we
// have to take care to:
//
// * check whether the index of the equation ID is less than
//   `ablocks`, and if it is, treat the data block in the expansion on
//   the right hand side as a block of zeros.
//
// * subtract `ablocks` from equation IDs to get the correct data
//   block to use when expanding a check block solution
//
// One more thing ...
//
// Auxiliary blocks are encoded as the first `ablocks` equations
// entered into the system, but as the last `ablocks` *variables*.

struct Decoder {
    mblocks : usize,		// message blocks
    ablocks : usize,		// auxiliary blocks
    coblocks: usize,		// mblocks + ablocks

    // These two arrays effectively implement the bipartite graph
    // logic

    variables  : Vec<Variable>,	// left side is mblocks and ablocks
    equations : Vec<Equation>,	// right side is what they expand to

    // maintain a list of variables that need visiting when an
    // equation becomes solved
    stack : Vec<VarID>,

    // count only unsolved mblocks? That makes sense.
    count_unsolveds : usize,
    done : bool,

    // We can determine if a variable is solved by looking at all
    // linked equations, and if we find one that has only that
    // variable on the left hand side, it is solved. As an
    // optimisation, we can explicitly store the status here:
    // is_var_solved : Vec<bool>
}


impl Decoder {

    fn new(mblocks : usize, ablocks : usize) -> Self {

        // variables table is fixed size
        let coblocks = mblocks + ablocks;
        let mut variables = Vec::with_capacity(coblocks);

        // allow headroom of ~11% in equations table. If space is
        // tight, we can reduce this ...
        let headroom = mblocks / 9;
        let mut equations = Vec::with_capacity(coblocks + headroom);

        // set up Variable struct for each mblock, ablock
        for _ in 0..coblocks {
            variables.push(Variable { 
                equations : HashSet::<EqID>::new()
            });
        }

        let count_unsolveds = mblocks;

        Self {
            mblocks, ablocks, coblocks, variables, equations,
            count_unsolveds,
            done : false,
            stack : Vec::with_capacity(40)
        }
    }


    fn add_aux_equations(&mut self, map : &AuxMapping) {

        for (ablock, list) in map.aux_to_mblocks.iter().enumerate() {

            let mut equation = Equation {
                lhs : HashSet::<VarID>::new(),
                rhs : Vec::<VarID>::new(),
            };
            equation.lhs.insert(self.mblocks + ablock);
            for mblock in list.iter() {
                equation.lhs.insert(*mblock);
                self.variables[*mblock].equations.insert(ablock);
            }
            self.equations.push(equation);
        }

    }

    // Add check block to graph. This is the main part of the decoder.
    fn add_check_equation(&mut self,
                          coblocks : Vec<VarID>,
                          step : bool) {

        let mut unknowns = coblocks.len();
        let mut equation = Equation {
                lhs : HashSet::<VarID>::new(),
                rhs : Vec::<VarID>::new(),
        };
        
        // substitute all previously-solved variables into the
        // equation
        for var in coblocks.iter() {

            let mut solved = false;
            
            // scan each equation that the variable appears in
            let equations = &self.variables[*var].equations;
            for eq in equations.iter() {
                if self.equations[*eq].lhs.len() == 1
                    && self.equations[*eq].lhs.contains(var) {
                    solved = true;
                    break;
                }
            }

            if solved {
                // do substitution (could be done above, actually)
                unknowns -= 1;
            }
        }
    }

}

