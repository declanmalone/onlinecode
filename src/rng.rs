//! # A random-number generator for Online Codes
//!
//! This implements required traits from the rand_core crate to get a
//! full-featured set of calling methods.
//!
//! Design is simply to feed the current state of the RNG (a 160-bit
//! number) into SHA-1.


// New approach
//
// I was going to be compatible with Rng, but distributions are a step
// too far. I want to focus on compatibility between Rust, C and Perl,
// so I will rewrite everything to use RngCore::next_u32 as the only
// rng call that I use. Everything else will be built on this...
//
// For a rand between 0 (included) and 1 (not included):
//
//     return ((rng.next_u32() * u32::MAX) >> 32) as f64
//
// When converting a table of f64 floating points that are in the
// range 0 <= float < 1 into [0 .. u32::MAX - 1]:
//
//     return ((float as u64 * u32::MAX)  >> 32) as u32
//
// Treat 1.0 in that case as u32::MAX (table sentinel)
//
// max_degree uses such a table, so it will call `rng.next_u32()` and
// use exactly the same logic as before, only working with unsigned
// 32-bit integers.
//
// floyd() needs integers in the range `0..j` (not including j). The
// operation is:
//
//     let pick : u32 = ((rng.next_u32() as u64 * j) >> 32).into()
//
// I think that I'll move every bit of code that uses random numbers
// into the same module. 
//
// For conformance testing, I'll generate lists of random numbers,
// calls to max_degree (which uses the probability distribution table)
// and calls to floyd (which generates ints in a range) and calculate
// the SHA-1 sum of the output (in a canonical format). Then I can
// simply check whether the hashes differ or not. That should be
// enough to be confident that the implementations are interoperable.
//
// Given a choice between special handling of 0xffffffff (by
// rerolling) and breaking backwards compatibility by using a better
// scheme, I'll go with the better scheme.

use crate::*;

use rand::{Rng, SeedableRng};
use rand_core::{RngCore, Error, impls};

use sha1::{Sha1, Digest};

// #[derive(Debug,Copy,Clone)]
pub struct SHA1Rng {

//    state : <SHA1Rng as rand::SeedableRng>::Seed,
    state : [u8; 20],
    
    // with 160 bits of state, we could spin up a new digest every
    // time we need a new random number, or we could consume 32 bits
    // of it at a time, only producing a new hash when we have run out
    // of bits. We can generate 5 32-bit values from 160 bits.

    state_bytes : usize,	// for SHA-1, 160 bits = 20 bytes
    avail_bytes : usize,	// count down to zero

    always_hash : bool,
}

impl SHA1Rng {
    // use fluent/builder-style setup of optional always_hash flag
    // since we can't pass any extra arguments to
    // SeedableRng::from_seed(). (Could also provide a generic
    // constructor here)
    pub fn always_hash(mut self) -> Self { self.always_hash = true; self }
    pub fn never_hash(mut self) -> Self  { self.always_hash = false; self }

    // move to next internal state: state <- sha1(state)
    pub fn next_hash(&mut self) {
	let hash = sha1::Sha1::digest(&self.state);
	self.state.copy_from_slice(&hash);
	self.avail_bytes = self.state_bytes;
    }

    // convenience constructors
    pub fn from_string(s : &String) -> Self {
	// 
	let hash = sha1::Sha1::digest(s.as_bytes());
	let mut output = [0u8; 20];
	output.copy_from_slice(&hash);
	Self::from_seed(output)
    }
    pub fn from_str(s : &str) -> Self {
	// 
	let hash = sha1::Sha1::digest(s.as_bytes());
	let mut output = [0u8; 20];
	output.copy_from_slice(&hash);
	Self::from_seed(output)
    }

    // TODO: implement floating-point rand that works like C/Perl
    // probably involves implementing custom Distribution to replace
    // Standard
}

impl SeedableRng for SHA1Rng {
    type Seed = [u8; 20];

    fn from_seed(seed: Self::Seed) -> Self {
	SHA1Rng {
	    state : seed,
	    state_bytes : 20,
	    avail_bytes : 20,
	    always_hash : true
	}
    }
}


impl RngCore for SHA1Rng {
    // prefer 32-bit numbers to avoid precision errors when
    // calculating. They will be upgraded to u64/f64

    // Notice that when avoiding hash, we return parts of the seed,
    // and then hash, whereas always_hash hashes it first. I'll have
    // to check this for compatibility with my Perl/C implementations.
    fn next_u32(&mut self) -> u32 {
	let mut val : u32;
	loop {
	    if self.avail_bytes < 4 || self.always_hash {
		self.next_hash();
	    }
	    // convert a (little endian) slice of state into a u32
	    let p = self.state_bytes - self.avail_bytes;
	    self.avail_bytes -= 4;
	    let array = [self.state[p], self.state[p+1],
			 self.state[p+2], self.state[p+3]];
	    val = u32::from_le_bytes(array);
	    // half way towards making compatible with C/Perl versions
	    if val != 0xffffffff { return val }
	}
    }

    fn next_u64(&mut self) -> u64 {
        self.next_u32() as u64
    }

    fn fill_bytes(&mut self, dest: &mut [u8]) {
        impls::fill_bytes_via_next(self, dest)
    }

    fn try_fill_bytes(&mut self, dest: &mut [u8]) -> Result<(), Error> {
        self.fill_bytes(dest);
	Ok(())
	    
    }
}


// Implement a Distribution 
use rand::distributions::Distribution;

/// Usage
///
/// ```rust
///
/// use rand::distributions::Distribution;
/// use rand::{Rng, SeedableRng};
///
/// use onlinecode::rng::{SHA1Rng, Compatible};
///
/// let zeros = [0u8; 20];
/// let mut rng = SHA1Rng::from_seed(zeros).always_hash();
///
/// let mut pick = rng.sample(Compatible {});
///
/// println!("Got value {0:.20} from rng", pick);
/// assert!(pick >= 0.0);
/// assert!(pick < 1.0);
///
/// // Alternative way of calling:
/// let distrib = Compatible {};
/// pick = distrib.sample(&mut rng);
///
/// ```
pub struct Compatible { }

impl Distribution<f64> for Compatible {

    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> f64 {
	// The normal Standard lops off some low bits. Here, I use a
	// full 32-bit rand, convert it to an f64
	let mut val : u32 = rng.gen();
	if val == u32::MAX {
	    // this is the least bad way of dealing with the
	    // possibility that the RNG sends us 0xffffffff.  It
	    // shouldn't, so a panic might be more in order, but I
	    // don't want (literally) random panics in the code.
	    eprintln!("Random number error; \"fixing\" it");
	    val = rng.gen();
	    // TODO: find a seed that should generate 0xffffffff as
	    // its random output. Use that as a test case.
	}
	let double : f64 = val.into();
	double / (u32::MAX as f64)
    }
}

#[cfg(test)]

mod tests {

    use rand::SeedableRng;
    use rand::Rng;
    use crate::rng::SHA1Rng;

    #[test]
    fn all_zeros_array() {
	let zeros = [0u8; 20];
	let rng = SHA1Rng::from_seed(zeros);
	assert_eq!(rng.always_hash, true);
    }

    #[test]
    fn build_always_hash() {
	let zeros = [0u8; 20];
	let rng = SHA1Rng::from_seed(zeros).always_hash();
	assert_eq!(rng.always_hash, true);
    }

    #[test]
    fn build_never_hash() {
	let zeros = [0u8; 20];
	let rng = SHA1Rng::from_seed(zeros).never_hash();
	assert_eq!(rng.always_hash, false);
    }

    #[test]
    fn get_some_zeros() {
	let zeros = [0u8; 20];
	let mut rng = SHA1Rng::from_seed(zeros).never_hash();

	// should we be able to get 5 zeros before re-hashing?
	let mut val : u32 = rng.gen();
	assert_eq!(val, 0, "rand 0 == 0");
	val = rng.gen();
	assert_eq!(val, 0, "rand 1 == 0");
	val = rng.gen();
	assert_eq!(val, 0, "rand 2 == 0");
	val = rng.gen();
	assert_eq!(val, 0, "rand 3 == 0");
	val = rng.gen();
	assert_eq!(val, 0, "rand 4 == 0");
	// don't know for certain that next value != 0, but good chance
	val = rng.gen();
	assert_ne!(val, 0, "rand 5 != 0");
	
    }

    #[test]
    fn get_single_zero() {
	let zeros = [0u8; 20];
	let mut rng = SHA1Rng::from_seed(zeros).always_hash();

	// should we be able to get 1 zero before re-hashing? No!
	// Don't know for certain that value != 0, but good chance
	let mut val : u32 = rng.gen();
	assert_ne!(val, 0, "always hash first value != 0");
    }


    
    
}
