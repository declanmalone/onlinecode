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

pub enum EquationType {
    AlreadySolved,
    // Solved: single variable on lhs, expansion on rhs
    Solved(VarID, Vec<VarID>),
    // Unsolved: several variables on lhs, expansion on rhs
    Unsolved(HashSet<VarID>, Vec<VarID>),
}
pub enum VariableType {
    Solved(EqID),
    Unsolved(HashSet<EqID>)
}

use std::iter::Iterator;

impl EquationType {
    fn iter(&self) -> impl Iterator<Item = &'_ VarID> {
        match self {
            Self::Unsolved(hash,_) => {
                hash.iter()
            },
            _ => {
                panic!("Can't insert into already-solved variable");
            }
            
        }
    }
}

impl VariableType {
    fn insert(&mut self, var : VarID) {
        match self {
            Self::Solved(_) => {
                panic!("Can't insert into already-solved variable");
            },
            Self::Unsolved(hash) => {
                hash.insert(var);
            }
        }
    }
    fn iter(&self) -> impl Iterator<Item = &'_ EqID> {
        match self {
            Self::Solved(_) => {
                panic!("Can't insert into already-solved variable");
            },
            Self::Unsolved(ref hash) => {
                hash.iter()
            }
        }
    }
}

pub struct Decoder {
    mblocks : usize,		// message blocks
    ablocks : usize,		// auxiliary blocks
    coblocks: usize,		// mblocks + ablocks

    // These two arrays effectively implement the bipartite graph
    // logic

    variables : Vec<VariableType>,
    equations : Vec<EquationType>,

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

// to-do
//
// at the cost of a little more space, we can use an enum to
// distinguish between unsolved variables and solved ones.
//
// Something like:
//
// enum Variable {
//   Solved(EqID),
//   Unsolved(HashSet<EqID>)
// }


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
            variables.push(
                //    Variable { equations : HashSet::<EqID>::new() }
                VariableType::Unsolved(HashSet::<EqID>::new())
            );
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

            // let mut equation = Equation { }
            let mut lhs = HashSet::<VarID>::new();
            let mut rhs = Vec::<VarID>::new();

            // note: the way that auxiliary blocks are generated means
            // that it is possible (though unlikely) that one is
            // generated with no links to any message blocks. Strictly
            // speaking, we should be storing it as "solved" (with
            // zero value), but since no variables refer to it anyway,
            // it doesn't affect the algorithm in any way.
            
            lhs.insert(self.mblocks + ablock);
            for mblock in list.iter() {
                lhs.insert(*mblock);
                self.variables[*mblock].insert(ablock);
            }
            self.equations.push(EquationType::Unsolved(lhs,rhs));
        }
    }


    // Add check block to graph.
    fn add_check_equation(&mut self,
                          coblocks : Vec<VarID>,
                          step : bool) -> (bool, Option<Vec<VarID>>) {

        // let mut unknowns = coblocks.len();
        // let mut equation = Equation {
        //         lhs : HashSet::<VarID>::new(),
        //         rhs : Vec::<VarID>::new(),
        // };

        // if we're already done, just return
        if self.done { return (true, None) }

        // substitute all previously-solved variables into the
        // equation
        let equation = self.new_equation(coblocks);

        match equation {
            EquationType::AlreadySolved => { (false, None) },
            EquationType::Solved(var,_) => {
                // insert as newly-solved
                let eq_position = self.equations.len();
                self.equations.push(equation);
                self.variables[var].insert(eq_position);

                // search graph starting from newly solved variable
                self.stack.push(var);
                // cascade will handle returning values
                self.cascade(step)
            },
            EquationType::Unsolved(ref hash,_) => {
                // insert as unsolved equation
                let eq_position = self.equations.len();

                // link unsolved variables to new equation
                for var in equation.iter() { 
                    self.variables[*var].insert(eq_position);
                }

                // store the equation
                self.equations.push(equation);
                (false, None)
            }
        }

    }

    // If a variable has been newly solved, it's possible that it now
    // allows other equations to be solved.
    //
    // We follow from the variable to the equations that it appears
    // in. First, we do substitution, which involves moving the
    // variable from the lhs to the rhs of the equation. Then we break
    // the link from the variable to the equation, since it is no
    // longer an unknown variable.
    //
    // If in the process of updating an equation we find that the
    // number of unknowns becomes 1, then the remaining variable on
    // the lhs of the equation also becomes solved. We queue up that
    // variable to have the above step repeated on it.
    //
    // This should work equally well for solved message blocks and
    // auxiliary blocks.

    fn cascade(&mut self, stepping : bool)
               -> (bool, Option<Vec<VarID>>) {

        let mut newly_solved = Vec::<VarID>::new();


        while let var = self.stack.pop() {

            //

            if stepping { return (self.done, None) }

        }

        ( self.done, None )
    }

    // There's a back-and-forth between variables and equations:
    //
    // we detect that a new or existing equation only has one unknown
    // on the lhs. That counts as solving the equation (and hence, a
    // variable).
    //
    // A newly-solved variable is substituted into all equations that
    // it appears in. This may cause one of those equation to become
    // solved.

    // There's also the case where a new check block equation comes in
    // and it contains only a single unknown.

    // I want to have a nice, clean division among the three pieces of
    // code above.

    // I was thinking of making an enum for solved/unsolved variables.
    // It might also be a good idea to do the same for equations.

    // think of it in terms of forward/back propagation...
    //
    // substituting a newly-solved variable into all equations it
    // appears in is like forward propagation
    //
    // if that equation becomes solved, then propagating the new info
    // deriving from it is kind of like back propagation (from
    // equations to variables)

    // on enum describing equation ... besides solved and unsolved, we
    // also have a third class, which represents "no new information".
    // These correspond to a number of unknowns of 0 (no new info), 1
    // (solved) and >1 (unsolved) in the code to add a new check block
    // equation. The case of 0 unknowns (over-specified/idempotent
    // equations) *could* be passed back to the caller to allow them
    // to verify that all the received check blocks are consistent
    // with each other, although it's easiest to just drop them and
    // assume that the sender is working correctly.


    /// Take a list of variables (block IDs) that comprise a check
    /// block and substitute any already-solved ones in, returning
    /// some kind of Option<EquationType> depending on the number of
    /// unknowns:
    ///
    /// * 0 unknowns &rarr; None (already solved)
    /// * 1 unknown &rarr; Some(Solved(VarID, Vec<VarID))
    /// * 1+ unknowns &rarr; Some(Unsolved(HashSet<VarID>, Vec<VarID>))


    // Hmm... might be better to get rid of already-solved enum type.
    // They will never get stored in the graph, and is only needed to
    // decide whether to drop a new check block. For that reason, I
    // think I should return Option<EquationType> here instead
    
    fn new_equation(&self, vars : Vec<VarID>) -> Option<EquationType> {

        // Use two passes. First determine how many unsolved vars
        // there are. Then, depending on whether it's 0, 1 or more
        // than 1, return different enum
        
        let mut count_unsolved = 0;
        for var in vars.iter() {
            // check to see if vars are solved
            match self.variables[*var] {
                VariableType::Solved(_)   => {},
                VariableType::Unsolved(_) => {
                    count_unsolved += 1;
                    if count_unsolved > 1 { break }
                }
            }
        }

        match count_unsolved {

            0 => { return None },
            1 => {
                let mut single_unsolved = 0;
                let mut rhs = Vec::new();
                for var in vars.iter() {
                    match self.variables[*var] {
                        VariableType::Solved(_)   => {
                            rhs.push(*var)
                        },
                        VariableType::Unsolved(_) => {
                            single_unsolved = *var;
                        }
                    }
                }
                return EquationType::Solved(single_unsolved, rhs);
            },
            _ => {
                let mut lhs = HashSet::new();
                let mut rhs = Vec::new();
                for var in vars.iter() {
                    match self.variables[*var] {
                        VariableType::Solved(_)   => {
                            rhs.push(*var)
                        },
                        VariableType::Unsolved(_) => {
                            lhs.insert(*var);
                        }
                    }
                    
                }
                return EquationType::Unsolved(lhs,rhs)
            }
        }
        
    }
}
    

// single-stepping
//
// 
