//! # Implement Online Codes, a type of fountain code
//!

pub mod probdist;
pub mod rng;
pub mod floyd;

// will implement the graph using an inefficient, dynamically
// allocated structure first, then a more efficient version using
// tables.
pub mod bipartite;
pub mod equations;

// use a trait to provide compatibility between the two
// implementations above.
pub mod compat;

// 
pub struct CodeSettings {
    pub e : f64,			// epsilon
    pub f : usize,

    pub mblocks  : usize,
    pub coblocks : usize,

    pub p : Vec<f64>,
}

// Difference between solving a message block and an aux block if only
// one variable is in the aux block?
//
// Check block must come in, in the form var = value
//
// If we normally count a left edge as being "solved" by checking the
// bone that it points to and seeing whether:
//
// * there is only a single variable on the left side of the equation
//   in the bone
//
// then for aux blocks, we may have to add an additional check to make
// sure that the variable is actually solved.
//
// OK. The way to do it is to create aux block bones (equations) so
// that they always include themselves in the list of unknown
// variables.
//
// 


// 
// psudocode ... with emphasis on lookups/changes to data.
// 
// receive new equation
//
// count unsolved variables (1)
//
// loop if count_unsolved == 0

// (1) could partition into solved, unsolved lists:

// (solved, unsolved) = count_eq_unsolved(&graph, variables)
// count_unsolved = size(unsolved);

// OR turn it into a bone-like structure:

// (count_unsolved, unsolved_list, expansion_list)
// = substitute_knowns(graph, equation)

// if count_unsolved > 1
//    insert new equation (2)
// else
//    apply new solution (3)
// 
// 
// (2) Assume that bones are going to represent edges and equations
// all in one bundle. We have to do two things to insert a new
// unsolved equation into the graph:
// 
// a) allocate storage for the new bone in our RH table (simply store
// a link to it, or clone it and push it onto the end of our table)
// 
// b) for all unsolved blocks, add a link from that block to the new
// bone.
// 
// At this point, just a note on the LH structure. I could avoid using
// an explicit "is solved" field on the LHS. If a left node only has a
// single "bone" (glorified edge), then we can look into it to count
// the number of unsolved elements it contains.
// 
// Basically, we need never deallocate bones individually. They will
// stay in the table as solutions. That's handy, because I don't have
// to work with Rc or Boxed types.
//
//  
// (3) is the bipartite graph algorithm. We alternate between going
// left and right:
//     
// * from left to right, we substitute a newly-solved variable into
//   all as-yet unsolved equations involving that variable, removing
//   those edges
// 
// * from right to left, if the substitution leaves us with only a
//   single unknown variable, this means that that variable is now also
//   newly-solved so we repeat the left-to-right step
// 
// This can be done recursively, or we can use an explicit queue/stack
// to avoid stack overflows.
//
// We could use an Option(usize) on the left to decide whether a
// variable has any rightward edges. However, due to the way the OC
// algorithm works, we will always have links. There are two levels of
// coding:
// 
// * construction of auxiliary blocks involves linking each variable
//   to q randomly-selected auxiliary blocks
// 
// * check blocks select from both message blocks and auxiliary blocks
// 
// When the decoder is initialised, before any check blocks have been
// received, we set up the auxiliary links. 
// 
//
// 
// Data structure to use for "lists"?
// 
// We have three list-like structures:
// 
// * on LHS, a list of "edges"
// * in Bone, a list of unknown variables
// * in Bone, a list of blocks in expansion
// 
// For the third of these, we can use a regular vector. We never query
// the contents of it, apart from iterating over it to decide which
// blocks will be XOR'd together.
//
// For the other two, we need to add elements and remove elements at
// random positions in the "list", so a regular vector is not a good
// idea. What we really want is a set-like structure.
// 
// Looking at std::collections, I can see ...
// 
// * HashMap, which suffers from poor performance when hashing a small
//   number of contiguous integers
// 
// * BTreeMap probably has the same problem, and we don't actually
//   need for the elements to be sorted as such
// 
// * HashSet is probably what I want
// 
// * BinaryHeap might also be an option
// 
// Actually, a thought occurred to me... I may actually need to look
// at the values in the expansion list, too. It seems that when we
// substitute a solution into the expansion list, we might end up with
// repeated terms, which need to be cancelled out.
//
// It depends on how we perform substitution, though. If we only add
// the block ID of the newly-solved variable, then it's impossible for
// that variable to already be in the expansion. So, not a problem.
// 
// I wasn't going to try and make the choice of set-like storage
// object be an option, but ... maybe. Maybe instead of making it
// generic, though, what about using "features"?
//
//    https://nnethercote.github.io/perf-book/hashing.html
// 


#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
