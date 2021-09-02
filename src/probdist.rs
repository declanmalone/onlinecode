//! Probability distribution

use crate::*;
use rand::{Rng};
use rand::rngs::StdRng;

use std::collections::HashSet;

// use higest-precision floats available
pub fn init_rand_table(codec : &CodeSettings) -> Result<Vec<f64>,&'static str> {

    let coblocks : usize = codec.coblocks;
    let epsilon  : f64   = codec.e;
    let f        : f64   = codec.f as f64;

    // Calculate the sum of the sequence:
    //
    //                1 + 1/F
    // p_1  =  1  -  ---------
    //                 1 + e
    //
    //
    //             F . (1 - p_1)
    // p_i  =  ---------------------
    //          (F - 1) . (i^2 - i)
    //
    // Since the i term is the only thing that changes for each p_i, I
    // optimise the calculation by keeping a fixed term involving only p
    // and f with a variable one involving i, then dividing as
    // appropriate.

    assert!(codec.f <= coblocks);

    let mut p = Vec::<f64>::with_capacity(codec.f);

    let p1 : f64 = 1.0 - (1.0 + 1.0/f) / (1.0 + epsilon);
    // assert!(p1 > 0.0); // epsilon crazy big?

    // simple (if unrealistic) case of f=1 or f=2
    if codec.f == 1 {
	p.push(1.0);
	return Ok(p)
    } else if codec.f == 2 {
	p.push(p1);
	p.push(1.0);
	return Ok(p)
    }

    // we know that f != 1, so f - 1 can never be zero:
    let pfterm : f64 = (1.0 - p1) * f / (f - 1.0);

    // push sum of terms, which should converge to 1
    let mut sum = p1;
    p.push(p1);

    // to avoid rounding/precision errors, set last term to 1
    for i in 2..codec.f {
	let iterm = i * (i - 1);
	sum += pfterm / iterm as f64;
	p.push(sum);
    }
    p.push(1.0);

    Ok(p)
}

/// Take table initialised above and use it to find how many blocks
/// (ie, the "degree") to include in a check block.
pub fn random_degree(codec : &CodeSettings, rng : &mut impl Rng) -> usize {
    let mut degree : usize = 0;
    let r : f64 = rng.gen();	// [0,1)
    while r > codec.p[degree] {
	degree += 1
    }
    degree + 1
}

pub fn max_degree(mut e : f64) -> usize {
    e /= 2.0;
    (2.0 * (e.ln() / (e - 1.0).ln())).ceil() as usize
}

pub fn count_aux(mblocks : usize, q : usize, e : f64) -> usize {
    let aux_blocks = (0.55 * e * (q * mblocks) as f64) as usize;
    if aux_blocks < q { q } else { aux_blocks }
}

fn eval_f(t : f64) -> usize {
    max_degree(1.0 / (1.0 + (-t).exp()))
}

pub fn recalculate_e(mblocks : usize, q : usize, e : f64) -> f64 {

    let ablocks = count_aux(mblocks, q, e);
    let coblocks = mblocks + ablocks;

    // l: left, r: right, m: middle
    let mut l = (1.0/e - 1.0).ln(); // natural log; `exp` to invert
    let mut r = l + 1.0;

    while eval_f(r) > coblocks {
	r += r - l
    }

    // binary search until we find an F <= n
    while r - l > 0.01 {
	let m = (l + r) / 2.0;
	if eval_f(m) > coblocks { l = m } else { r = m}
    }

    1.0 / (1.0 + (-r).exp())
}
