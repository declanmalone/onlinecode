//! # Implement Fisher-Yates and Floyd's algorithm




// Basic idea: select p elements randomly from a set of n items,
// without replacement.
//
// It appears that the rand crate has the feature that I want, and I
// can probably pass in my own RNG to it. I'm pretty sure that it uses
// the Fisher-Yates algorithm internally, but I need to be sure that
// give the same RNG and seed that it will generate the same set of
// elements on all machines. I could test it to make sure that it
// using that algorithm, and then make that version of the rand crate
// a dependency. However, it's easier to just have my own version.
//
// The two algorithms here are slightly different
//
// Fisher-Yates:
//
// Construct an array containing all possible elements, then
// (potentially) swap elements around.
//
// Floyd:
//
// Maintain a set of selected elements. Add selected elements to the
// set.
//
// Obviously, there's more to both algorithms. The most important
// point as a user is whether they need to set up a fresh array each
// time.

// Actually, looking at the `shuffle` and `partial_shuffle` routines
// in rand (0.3.8) do implement Fisher-Yates, as expected.


// https://stackoverflow.com/questions/2394246/algorithm-to-select-a-single-random-combination-of-values

use rand::{Rng};
use rand::rngs::StdRng;

use std::collections::HashSet;

/// # Pick k distinct elements (indices) uniformly from 0 <= usize < n
///
/// Floyd's algorithm:
///
/// ```pseudocode
///     initialize set S to empty
///     for J := N-K + 1 to N do
///        T := RandInt(1, J)
///        if T is not in S then
///            insert T in S
///        else
///            insert J in S
/// ```
/// 
/// The above algorithm above works for 1-based arrays. Also, the top
/// end of the loop (and `RandInt`) range is included. To convert to
/// 0-based array and Rust's half-open `for`/`gen_range()`, must be
/// sure that:
/// 
/// * loop looks at `k` items in total
/// * `RandInt` spans `j` elements
/// * all off-by-one adjustments are correct
///
pub fn floyd_usize(rng : &mut impl Rng, k : usize, n : usize)
		   -> HashSet<usize> {

    let mut set = HashSet::<usize>::with_capacity(n);
    let j_range = n - k + 1..n + 1;
    assert_eq!(j_range.len(), k);

    for j in j_range {
	let r_range = 0..j;
	assert_eq!(r_range.len(), j);

	let t : usize = rng.gen_range(r_range);
	if set.contains(&t) {
	    eprintln!(" {} is already in set! Adding {} instead.", t, j - 1);
	    set.insert(j - 1);
	} else {
	    eprintln!(" {} not in set! Adding it.", t);
	    set.insert(t);
	}
    }
    set
}

#[cfg(test)]


mod tests {
    use super::*;
    use rand::thread_rng;
    //use crate::floyd::*;

    #[test]
    // pick 1 item from 1 option
    fn highlander() {
	let mut rng = thread_rng();
	let set = floyd_usize(&mut rng, 1, 1);
	assert_eq!(set.len(), 1);
	assert!(set.contains(&0));
    }

    #[test]
    #[should_panic]
    fn one_too_many() {
	let mut rng = thread_rng();
	let set = floyd_usize(&mut rng, 2, 1);
    }

    #[test]
    // pick k items from k options (highly likely (p = 1 - (1/256!))
    // to exercise other branch of if statement)
    fn k_from_k() {
	let mut rng = thread_rng();
	let set = floyd_usize(&mut rng, 256, 256);
	assert_eq!(set.len(), 256);
	for val in 0usize..256 {
	    assert!(set.contains(&val));
	}
    }

    #[test]
    // pick 0 items from 1 option
    fn none_thank_you() {
	let mut rng = thread_rng();
	let set = floyd_usize(&mut rng, 0, 1);
	assert_eq!(set.len(), 0);
    }

    // Shouldn't need any more tests, I think. No need to prove that
    // the picks are uniform.
}
