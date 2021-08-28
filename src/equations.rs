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
    // dropping AlreadySolved in favour of using Option<T>
    // AlreadySolved,
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
            Self::Unsolved(hash,_) => { hash.iter() },
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
    // logic. Both types come in solved and unsolved flavours.

    variables : Vec<VariableType>,
    equations : Vec<EquationType>,

    // maintain a list of variables that need visiting when an
    // equation becomes solved
    stack : Vec<VarID>,

    // count only unsolved mblocks? That makes sense.
    count_unsolveds : usize,
    done : bool,
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

        eprintln!("Decoder adding {} aux blocks", map.aux_to_mblocks.len());

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
            // link aux variable to equation
            self.variables[self.mblocks + ablock].insert(ablock);
            // store the equation
            self.equations.push(EquationType::Unsolved(lhs,rhs));
        }
    }


    /// Add check block to graph. 
    fn add_check_equation(&mut self,
                          coblocks : Vec<VarID>, step : bool)
                          -> (bool, usize, Option<Vec<VarID>>) {

        // let mut unknowns = coblocks.len();
        // let mut equation = Equation {
        //         lhs : HashSet::<VarID>::new(),
        //         rhs : Vec::<VarID>::new(),
        // };

        // if we're already done, just return
        if self.done { return (true, self.stack.len(), None) }

        // substitute all previously-solved variables into the
        // equation
        let equation = self.new_equation(coblocks);

        if equation.is_none() {
            eprintln!("add_check_equation classified as None");
            return (false, self.stack.len(), None)
        }

        let equation = equation.unwrap();

        match equation {
            EquationType::Solved(var,_) => {
                // insert as newly-solved
                eprintln!("add_check_equation classified as Solved");
                let eq_position = self.equations.len();
                self.equations.push(equation);
                self.variables[var].insert(eq_position);

                // search graph starting from newly solved variable
                self.stack.push(var);

                // At this point, we've added a solved *equation*, and
                // linked the newly-solved variable to it. What
                // cascade() does is to check if the variable can be
                // substituted into other equations. It's responsible
                // for marking the *variable* as solved afterwards.
                self.cascade(step)
            },
            EquationType::Unsolved(ref hash,_) => {
                // insert as unsolved equation
                eprintln!("add_check_equation classified as Unsolved");
                let eq_position = self.equations.len();

                // link unsolved variables to new equation
                for var in equation.iter() { 
                    self.variables[*var].insert(eq_position);
                }

                // store the equation
                self.equations.push(equation);
                (false, self.stack.len(), None)
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
               -> (bool, usize, Option<Vec<VarID>>) {

        // return list of newly-solved variables
        let mut newly_solved = Vec::<VarID>::new();

        while let Some(var) = self.stack.pop() {

            // var is always a solved variable at this point
            newly_solved.push(var);

            // we know that the variable is solved, but we don't know
            // which equation solved it. After scanning the list of
            // equations that the variable appears in, we should find
            // exactly one equation that matches.
            let mut found_solved = 0;
            let mut solved_equation = 0;

            for eq_id in self.variables[var].iter() {
                match &mut self.equations[*eq_id] {

                    EquationType::Solved(v,rhs) => {
                        debug_assert_eq!(*v, var);
                        found_solved += 1;
                        solved_equation = *eq_id;
                    },
                    EquationType::Unsolved(hash, rhs) => {
                        // Need to do a little dance here. If the
                        // equation would become solved, we need to
                        // replace it with a ::Solved flavour. The
                        // borrow checker will not be happy, though. I
                        // may need to use an if let form instead of a
                        // match.

                        // Anyway, the gist is...
                        //
                        // if there are two entries in hash, delete
                        // var from it, placing it into the rhs, and
                        // use the remaining variable in the hash to
                        // create a Solved EquationType (using
                        // remaining variable and updated rhs). Also
                        // add the remaining variable to the stack,
                        // since it is now newly-solved. Then write
                        // the new Solved flavour back to the table.
                        //
                        // or else ... remove var from hash and append
                        // it to rhs

                        match hash.len() {
                            2 => {
                                let mut vars = hash.iter();
                                let mut other = vars.next().unwrap();
                                if *other == var {
                                    other = vars.next().unwrap()
                                }
                                rhs.push(var);
                                self.stack.push(*other);
                                // the contentious bit (I think to_vec() copies)
                                self.equations[*eq_id] =
                                    EquationType::Solved(*other,rhs.to_vec());
                            },
                            1 => {
                                // This case shouldn't happen since
                                // unsolved equation with a single
                                // unknown should have been replaced
                                // with a solved variant.
                                panic!("Internal Error: Unsolved eq {} wrong",
                                       eq_id);
                            },
                            _ => { 
                                hash.remove(&var);
                                rhs.push(var);
                            }
                        };
                    }
                }

            } // for eq_id in self.variables.iter()

            // variable also need to be marked as solved ...
            debug_assert_eq!(found_solved, 1);
            match &self.variables[var] {
                VariableType::Solved(_eq_id) => {
                    panic!("Internal error: var {} was solved twice", var);
                },
                VariableType::Unsolved(_hash) => {
                    self.variables[var] = VariableType::Solved(solved_equation)
                },
            }

            // update self's count of unsolved message blocks
            if var < self.mblocks {
                self.count_unsolveds -= 1;
                if self.count_unsolveds == 0 {
                    self.done = true;
                    break;
                }
            }

            // I should probably also be returning the number of items
            // left in the stack if I want single-stepping to be
            // useful.
            //
            // When single-stepping, call add_check_equation() once
            // with the stepping option = true. Then while the second
            // return value (the number of variables pending) is >0,
            // call cascade(). This should solve equations/variables
            // in the same order as a non-stepping call to
            // add_check_equation().
            if stepping { break }

        }

        ( self.done, self.stack.len(), Some(newly_solved) )
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
                    // if count_unsolved > 1 { break }
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
                return Some(EquationType::Solved(single_unsolved, rhs))
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
                return Some(EquationType::Unsolved(lhs,rhs))
            }
        }
        
    }

    fn var_solution(&self, var : VarID)
                    -> Option<(Option<usize>, Vec<EqID>)> {

        // actually ... we don't need initial, do we? Isn't the
        // expansion for the variable completely on the RHS of the
        // equation? No, it's not...
        
        // var -> eq, and eq contains rhs (and a copy of var)
        //
        // if eq < ablocks, it represents an auxiliary block, so the
        // initial block should be zero/not-present
        //
        // if eq >= ablocks, then it has an implicitly associated
        // check block, which is eq - ablocks

        // Or should I be checking whether var < mblocks instead?
        // No, I should test eq < ablocks.
        //
        // Variables in general can be solved by auxiliary equations
        // or check equations, so we look at the equation number, not
        // the variable number to determine the correct expansion.

        let mut initial = None;
        
        match self.variables[var] {
            VariableType::Solved(eq_id) => {
                
                // if var < self.mblocks {
                //     initial = Some(eq_id);
                // }
                match &self.equations[eq_id] {
                    EquationType::Solved(lhs,rhs) => {
                        debug_assert_eq!(*lhs, var); // internal error
                        // let mut vec = Vec::new();
                        if eq_id >= self.ablocks {
                            // variable solved by check block
                            // vec.push(eq_id - self.ablocks);
                            initial = Some(eq_id - self.ablocks);
                        }
                        // vec.extend(rhs.to_vec());
                        Some((initial, rhs.to_vec()))
                    },
                    _ => {
                        // caller error
                        panic!("eq[{}] not marked as solved", eq_id)
                    }
                }
            },
            _ => { None }
        }
    }
}
    

#[cfg(test)]
mod test_decoder {

    use super::*;

    #[test]
    fn single_unknown_no_aux() {
        let mut d = Decoder::new(1,0);
        // check block with a single unknown
        let (done,pending,solved)
            = d.add_check_equation(vec![0usize], false);

        assert_eq!(done, true); // last block was solved
        assert_eq!(pending, 0); // cascade didn't solve extra
        assert_eq!(solved.is_none(), false); // got Some(Vec)

        // unwrap the vector of solved vars and ensure 0 is the only
        // item in it
        let solved = solved.unwrap();
        assert_eq!(solved[0], 0);
        assert_eq!(solved.len(), 1);
    }
    
    #[test]
    fn two_unknowns_no_aux_no_step() {
        let mut d = Decoder::new(2,0);
        // check block with two unknowns (blocks 0, 1)
        let (done,pending,solved)
            = d.add_check_equation(vec![0usize,1], false);

        assert_eq!(done, false); // nothing solved
        assert_eq!(pending, 0); // cascade didn't solve extra
        assert!(solved.is_none()); // None instead of Some(Vec)

        // a check block with either 0 or 1 by itself should
        // solve both due to cascade.
        let (done,pending,solved)
            = d.add_check_equation(vec![0usize], false);

        assert_eq!(done, true); // all solved
        assert_eq!(pending, 0); // nothing left in pending
        assert!(!solved.is_none()); // Some(Vec)

        // unwrap the vector of solved vars. It should have [0,1] in
        // that order

        let solved = solved.unwrap();
        assert_eq!(solved, [0,1]);
        // assert_eq!(solved.len(), 2);

        // confirm that solutions are actually correct
        // var[0] -> eq[1], containing (1, empty vec)
        // (interpretation: var_0 = check_block_1, no other xors)

        match d.variables[0] {
            VariableType::Solved(var0_eq) => {
                assert_eq!(var0_eq, 1);
                match &d.equations[var0_eq] {
                    EquationType::Solved(lhs,rhs) => {
                        assert_eq!(*lhs, 0); // solves variable 0
                        assert_eq!(*rhs, vec![]);
                    },
                    _ => { panic!("eq[1] not marked as solved") }
                }
            },
            _ => { panic!("var[0] not marked as solved") }
        }

        // var[1] -> eq[0], containing (0, vec![0])
        // (interpretation: var_1 = check_block_0 xor var_0)

        match d.variables[1] {
            VariableType::Solved(var1_eq) => {
                assert_eq!(var1_eq, 0);
                match &d.equations[var1_eq] {
                    EquationType::Solved(lhs,rhs) => {
                        assert_eq!(*lhs, 1); // solves variable 1
                        assert_eq!(*rhs, vec![0]);
                    },
                    _ => { panic!("eq[0] not marked as solved") }
                }
            },
            _ => { panic!("var[1] not marked as solved") }
        }
    }


    // make sure that solving via auxiliary blocks works as expected
    //
    // Setup (4 cases):
    //
    // let aux_0 = msg_0
    // let chk_0 = (msg_0 or aux_0) + msg_1
    // let chk_1 = (msg_0 or aux_0)
    //
    // If we receive a new check block solving either aux_0 or msg_0
    // we should be able to solve msg_1.
    //
    // Code these as separate tests

    // Case 1:
    // chk_0 = msg_0 + msg_1
    // chk_1 = msg_0
    //    
    // Doesn't use aux for solution (so it's like two unknowns test as
    // above), but does check that add_aux_equations() works correctly
    // and that array indices in solutions are still correct.
    // #[test]
    fn solve_via_aux_case_1() {

        // system with two unknown message blocks (0, 1) and one aux
        let mut d = Decoder::new(2,1);

        // first, add auxiliary mapping (aux -*-> msg)
        let aux_map = AuxMapping { aux_to_mblocks : vec![vec![0]] };
        d.add_aux_equations(&aux_map);

        // Test that system is set up as expected. We shouldn't need
        // to repeat this part of the testing for other cases.

        match &d.variables[0] {
            VariableType::Unsolved(hash) => {
                assert_eq!(hash.len(), 1);
                assert!(hash.contains(&0)); // eq0 = aux0 + msg0
                match &d.equations[0] {
                    EquationType::Unsolved(hash, vec) => {
                        assert_eq!(hash.len(), 2);
                        assert!(hash.contains(&0)); // msg0
                        assert!(hash.contains(&2)); // aux0
                        assert_eq!(vec.len(), 0);   // rhs empty
                    },
                    _ => { panic!("aux0 equation wrongly set as solved")
                    }
                }
            },
            _ => { panic!("msg0 wrongly set as solved") }
        }
        match &d.variables[1] {
            VariableType::Unsolved(hash) => {
                assert_eq!(hash.len(), 0);
            },
            _ => { panic!("msg1 wrongly set as solved") }
        }
        match &d.variables[2] {
            VariableType::Unsolved(hash) => {
                assert_eq!(hash.len(), 1);
                assert!(hash.contains(&0)); // eq0 = aux0 + msg0
                // contents of equation already checked above
            },
            _ => { panic!("aux0 wrongly set as solved") }
        }

        // add chk0 = msg0 + msg1
        let (done,pending,solved)
            = d.add_check_equation(vec![0usize,1], false);
        assert_eq!(done, false); // nothing solved
        assert_eq!(pending, 0); // cascade didn't solve extra
        assert!(solved.is_none()); // None instead of Some(Vec)

        // add chk1 = msg0
        let (done,pending,solved)
            = d.add_check_equation(vec![0usize], false);
        assert_eq!(done, true); // all solved

        // Unfortunately, HashSet::iter() returns keys in
        // nondeterministic order, so testing is not so
        // straightforward. 
        
        assert_eq!(pending, 1); // aux should be pending?
        // Actually, it depends on how hash keys are traversed. The
        // order of keys from iterating HashMap/HashSet seems to be
        // non-deterministic.

        assert!(!solved.is_none()); // Some(Vec)
        let solved = solved.unwrap();

        // order of solutions should be msg0, msg1
        assert_eq!(solved, [0,1]);

        // check contents of solved variables ...
        //
        // There's a fair bit of boilerplate code involved in
        // following variables to equations and then destructuring
        // things. I'll add a new method to help with that...

        // the method needs to return an optional equation number,
        // which corresponds to Some(received check block) or None in
        // the case that 
        
        let rhs = d.var_solution(0).unwrap();

        // Some(1) means check block 1
        // the empty vector means no solved variables to xor in
        assert_eq!(rhs, (Some(1),vec![]));
        
    }

    // Rewrite test cases above to eliminate nondeterminism
    //
    // Forget about msg1 completely
    //
    // R1: (aux) aux0 = msg0
    // R2a: chk0 = aux0
    // R2b: chk0 = msg0

    #[test]
    fn deterministic_aux_case_1() {

        // system with one unknown message blocks 0 and one aux
        let mut d = Decoder::new(1,1);

        // first, add auxiliary mapping (aux -*-> msg)
        let aux_map = AuxMapping { aux_to_mblocks : vec![vec![0]] };
        d.add_aux_equations(&aux_map);

        // Test that system is set up as expected. We shouldn't need
        // to repeat this part of the testing for other cases.

        match &d.variables[0] {
            VariableType::Unsolved(hash) => {
                assert_eq!(hash.len(), 1);
                assert!(hash.contains(&0)); // eq0 = aux0 + msg0
                match &d.equations[0] {
                    EquationType::Unsolved(hash, vec) => {
                        assert_eq!(hash.len(), 2);
                        assert!(hash.contains(&0)); // msg0
                        assert!(hash.contains(&1)); // aux0
                        assert_eq!(vec.len(), 0);   // rhs empty
                    },
                    _ => { panic!("aux0 equation wrongly set as solved")
                    }
                }
            },
            _ => { panic!("msg0 wrongly set as solved") }
        }
        match &d.variables[1] {
            VariableType::Unsolved(hash) => {
                assert_eq!(hash.len(), 1);
                assert!(hash.contains(&0)); // eq0 = aux0 + msg0
                // contents of equation already checked above
            },
            _ => { panic!("aux0 wrongly set as solved") }
        }

        // add chk0 = aux0
        let (done,pending,solved)
            = d.add_check_equation(vec![1usize], false);
        assert_eq!(done, true); // everything solved
        assert_eq!(pending, 0); // cascade didn't solve extra
        assert!(!solved.is_none()); // Some(Vec)

        let solved = solved.unwrap();

        // order of solutions should be aux0, msg0
        assert_eq!(solved, [1,0]);

        // test solution of aux block first (values in the returned
        // array are all check block IDs)
        let rhs = d.var_solution(1).unwrap();

        assert_eq!(rhs, (Some(0), vec![])); // = chk0

        // using the above, we can set chk1 (ie, aux0) = chk0

        // That then tallies with the solution msg1 = chk1 (=chk0)
        let rhs = d.var_solution(0).unwrap();

        assert_eq!(rhs, (None, vec![1]));
        
    }
    
    #[test]
    fn deterministic_aux_case_2() {

        // system with one unknown message blocks 0 and one aux
        let mut d = Decoder::new(1,1);

        // first, add auxiliary mapping (aux -*-> msg)
        let aux_map = AuxMapping { aux_to_mblocks : vec![vec![0]] };
        d.add_aux_equations(&aux_map);

        // Test that system is set up as expected. We shouldn't need
        // to repeat this part of the testing for other cases.

        match &d.variables[0] {
            VariableType::Unsolved(hash) => {
                assert_eq!(hash.len(), 1);
                assert!(hash.contains(&0)); // eq0 = aux0 + msg0
                match &d.equations[0] {
                    EquationType::Unsolved(hash, vec) => {
                        assert_eq!(hash.len(), 2);
                        assert!(hash.contains(&0)); // msg0
                        assert!(hash.contains(&1)); // aux0
                        assert_eq!(vec.len(), 0);   // rhs empty
                    },
                    _ => { panic!("aux0 equation wrongly set as solved")
                    }
                }
            },
            _ => { panic!("msg0 wrongly set as solved") }
        }
        match &d.variables[1] {
            VariableType::Unsolved(hash) => {
                assert_eq!(hash.len(), 1);
                assert!(hash.contains(&0)); // eq0 = aux0 + msg0
                // contents of equation already checked above
            },
            _ => { panic!("aux0 wrongly set as solved") }
        }

        // add chk0 = msg0
        let (done,pending,solved)
            = d.add_check_equation(vec![0usize], false);
        assert_eq!(done, true); // everything solved
        assert_eq!(pending, 1); // cascade detected done, w/o solving aux
        assert!(!solved.is_none()); // Some(Vec)

        let solved = solved.unwrap();

        // order of solutions should be msg0
        assert_eq!(solved, [0]);

        // test solution of aux block first (values in the returned
        // array are all check block IDs)
        let rhs = d.var_solution(0).unwrap();

        assert_eq!(rhs,(Some(0), vec![])); // = chk0
        
    }
    
    // The first deterministic case above showed an aux block solving
    // a msg block, but because of how `done` is handled, the second
    // case didn't fully show what happens then a msg block gets
    // solved first. Extend that by adding in a second, unrelated
    // message block.
    
    #[test]
    fn deterministic_aux_case_3() {

        // system with one unknown message blocks 0 and one aux
        let mut d = Decoder::new(2,1);

        // first, add auxiliary mapping (aux -*-> msg)
        let aux_map = AuxMapping { aux_to_mblocks : vec![vec![0]] };
        d.add_aux_equations(&aux_map);

        // Test that system is set up as expected. We shouldn't need
        // to repeat this part of the testing for other cases.

        match &d.variables[0] {
            VariableType::Unsolved(hash) => {
                assert_eq!(hash.len(), 1);
                assert!(hash.contains(&0)); // eq0 = aux0 + msg0
                match &d.equations[0] {
                    EquationType::Unsolved(hash, vec) => {
                        assert_eq!(hash.len(), 2);
                        assert!(hash.contains(&0)); // msg0
                        assert!(hash.contains(&2)); // aux0
                        assert_eq!(vec.len(), 0);   // rhs empty
                    },
                    _ => { panic!("aux0 equation wrongly set as solved")
                    }
                }
            },
            _ => { panic!("msg0 wrongly set as solved") }
        }
        // check aux block
        match &d.variables[2] {
            VariableType::Unsolved(hash) => {
                assert_eq!(hash.len(), 1);
                assert!(hash.contains(&0)); // eq0 = aux0 + msg0
                // contents of equation already checked above
            },
            _ => { panic!("aux0 wrongly set as solved") }
        }

        // add chk0 = msg0
        let (done,pending,solved)
            = d.add_check_equation(vec![0usize], false);
        assert_eq!(done, false); // msg1 not solved yet
        assert_eq!(pending, 0); // cascade doesn't do done
        assert!(!solved.is_none()); // Some(Vec)

        let solved = solved.unwrap();

        // order of solutions should be msg0,aux0
        assert_eq!(solved, [0,2]);

        // test solution of aux block first (values in the returned
        // array are all check block IDs)
        let rhs = d.var_solution(0).unwrap();

        assert_eq!(rhs, (Some(0), vec![]) ); // = chk0

        // aux0 should be solved by msg0
        let rhs = d.var_solution(2).unwrap();

        assert_eq!(rhs, (None, vec![0])); // = chk0
        
    }


    // Random testing
    //
    // The Online Code algorithm specifies a particular construction
    // of auxiliary blocks and check blocks. However, we can simplify
    // that for the purposes of random testing on the decoder. It will
    // take longer to fully decode the original message, but it should
    // terminate at some point.
    //
    // There are two things to test in this mode:
    //
    // * that new solutions are composed only of already-solved
    //   variables; and
    //
    // * when we do the XORs that the decoder tells us to do, we
    //   actually recover the correct values
    //
    // If this fails, it probably won't be easy to find out why it
    // failed, and it won't be possible to reproduce the error. On the
    // other hand, if it succeeds, we can have very high confidence
    // that the decoder is bug-free.

    // Will need some support routines which can be considered toy
    // versions of the proper Online Code algorithm

    use rand::{Rng,thread_rng};

    fn random_message(size : usize) -> Vec<usize> {
        let mut rng = thread_rng();
        let mut vec = Vec::with_capacity(size);
        for _ in 0..size {
            vec.push(rng.gen())
        }
        vec
    }

    use crate::floyd::*;

    fn generate_check_block(max_picks : usize, from : usize)
                            -> HashSet<usize> {

        let mut rng = thread_rng();
        let how_many = rng.gen_range(1..max_picks + 1);
        assert!(how_many > 0);
        assert!(how_many <= max_picks);

        floyd_usize(&mut rng, how_many, from)
    }

    fn hashset_to_vec(set : &HashSet<usize>) -> Vec<usize> {
        set.iter().cloned().collect::<Vec<_>>()
    }

    // ablocks each comprising a random selection of `m_per_a` mblocks
    fn toy_aux_mapping(mblocks : usize, ablocks : usize, m_per_a : usize)
                       -> Vec<Vec<usize>> {

        let mut rng = thread_rng();
        let mut vec = Vec::with_capacity(ablocks);
        for _ in 0..ablocks {
            let aux_map = floyd_usize(&mut rng, m_per_a, mblocks);
            vec.push(hashset_to_vec(&aux_map))
        }
        vec
    }

    #[test]
    fn test_toy_codec() {

        let mblocks = 75;
        let ablocks = 25;
        let m_per_a = 6;
        let max_picks = 8;      // max blocks to put in check block

        // coding side
        //
        let mut message = random_message(mblocks);

        // aux blocks stored at the end of message
        // let aux_blocks = Vec::with_capacity(ablocks);

        // check_blocks can be shared between coder and decoder
        let mut check_blocks = Vec::<usize>::with_capacity(200);

        let aux_mapping = toy_aux_mapping(mblocks, ablocks, m_per_a);

        // calculate aux blocks and append them to message for easier
        // check block creation
        for aux_list in aux_mapping.iter() {
            let mut sum = 0usize;
            for mblock in aux_list {
                sum ^= message[*mblock];
            }
            message.push(sum);
        }

        // decoding side
        let mut d = Decoder::new(mblocks, ablocks);
        d.add_aux_equations(& AuxMapping { aux_to_mblocks : aux_mapping });

        // validate ordering of solutions
        let mut solvedp = vec![false ; mblocks + ablocks];

        while !d.done {

            let check_vec = hashset_to_vec(
                &generate_check_block(max_picks, mblocks + ablocks));
            let mut check_val = 0;
            for index in check_vec.iter() {
                check_val ^= message[*index]
            }

            eprintln!("Check block {} comprising: {:?}, value {}",
                      check_blocks.len(), check_vec,  check_val);

            // sender and receiver can end up disagreeing on what the
            // current check block number is if the receiver drops the
            // block because it provides no new info. To fix this,
            // I'll restrict changes to just the codec code here...

            let mut useless = true;
            for maybe_known in check_vec.iter() {
                if !solvedp[*maybe_known] {
                    useless = false;
                    break;
                }
            }
            if useless {
                eprintln!("Not storing a useless block");
                continue
            } else {
                check_blocks.push(check_val);
            }

            let (done, pending, solved) =
                d.add_check_equation(check_vec, false);

            if solved.is_none() {
                // see above for proper fix for "useless" check blocks
                // check_blocks.pop();
                continue
            }

            let solved = solved.unwrap();

            for var in solved.iter() {
                // the first, optional part of return is a check block
                let (chk, vars) = d.var_solution(*var).unwrap();

                eprintln!("Checking solution for var {}: ({:?},{:?})",
                          *var, chk, vars);

                let mut sum = match chk {
                    None => 0,
                    Some(x) => {
                        let value = check_blocks[x];
                        eprintln!("Check {} value is {}", x, value);
                        value
                    }
                };

                // the remaining values are variables
                for v in vars.iter() {
                    // make sure the variables were already solved
                    assert_eq!(solvedp[*v], true);
                    // that being the case, and because we check each
                    // new var result (below), this is allowed: (in
                    // practice, decoder will have its own structure)
                    eprintln!("adding message {} value {}",
                              *v, message[*v]);
                    sum ^= message[*v];
                }

                eprintln!("Solving var {}", *var);

                // mark var as solved
                solvedp[*var] = true;

                // compare result to original
                assert_eq!(sum, message[*var]);
            }

        }
    }
}
