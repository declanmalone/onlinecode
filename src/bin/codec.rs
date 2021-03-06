// Codec based on toy codec implemented in unit tests of equations.rs
//
// We use the proper versions of the following, as described in the
// original papers:
//
// * codec parameters (e, q, mblocks)
// * auxiliary mapping (attach each message block to q aux blocks)
// * probability distribution table for deciding how many blocks
//   comprise a check block
// * random numbers are portably deterministic based on supplied seed
//
// Since this is only intended to test the correct functioning of the
// encoding and decoding algorithms, the block size is limited to
// being a u32 value. For sending and receiving files, larger blocks
// would be used.
//

// Add this to allow (done,_pending, solved) = ... (no `let`)
#![feature(destructuring_assignment)]

use clap::{Arg, App};
use sha1::{Sha1, Digest};
use rand::{Rng, SeedableRng};

// code is kind of scattered all over the place at the moment
use onlinecode::*;
use onlinecode::rng::*;
use onlinecode::compat::*;
use onlinecode::floyd::*;
use onlinecode::probdist::*;
use onlinecode::equations::*;

// Simulate sending receiving a file, except instead of blocks of 512,
// 1024, or whatever bytes, we just use one block = one u32
//
// In order for tests to be repeatable, we use the same sample file
// regardless of codec parameters.

fn sample_file(mblocks : usize) -> Vec<u32> {
    let mut vec = Vec::with_capacity(mblocks + mblocks / 5);
    for b in 0..mblocks  {
        vec.push(b as u32)
    }
    vec
}

fn main() {

    let matches = App::new("codec")
        .version("1.0")
        .author("Declan Malone <idablack@users.sourceforge.net>")
        .about("Test Online Code encoding/decoding")
        .args_from_usage(
            "-n                   'Use null seed (default)'
             -r                   'Use random seed'
             -s=[string]          'Use specific seed (quoted string)'
             -m=<int>             'number of message blocks'
             -e=[int]             'epsilon setting (default 0.01)'
             -q=[int]             'q setting (default 3)'
             -E=[int]             'tuning parameter (default 0.55)'
             ")
        .get_matches();

    // set up variables from args or default
    let mut e : f64 = if let Some(num) = matches.value_of("e") {
        num.parse().expect("Invalid value for -e option")
    } else {
        0.01
    };

    let q : usize = if let Some(num) = matches.value_of("q") {
        num.parse().expect("Invalid value for -q option")
    } else {
        3
    };

    let big_e : f64 = if let Some(num) = matches.value_of("E") {
        num.parse().expect("Invalid value for -E option")
    } else {
        0.55
    };

    let mblocks : usize = matches.value_of("m").unwrap().parse().unwrap();

    // check the above code
    eprintln!("Options:\ne: {}, q: {}, mblocks: {}, E: {}",
              e, q, mblocks, big_e);


    // The -n, -r, and -s options are mutually-exclusive

    // Is there a way to make the last instance be the winner using
    // clap?
    //
    // I'll just quit with an error if more than one was provided
    
    // default (null) seed. 
    let mut starting_seed = [0u8; 20];
    let mut seed_opts = 0;

    // use Rust's RNG to generate random seeds (both for -r option and
    // when creating check blocks later)
    let mut rust_rng = rand::thread_rng();

    if matches.is_present("n") {
        // no need to init
        eprintln!("Using null seed");
        seed_opts += 1;
    }
    
    if matches.is_present("r") {
        eprintln!("Using random seed");
        let random_bytes = rust_rng.gen::<[u8; 20]>();
        starting_seed.copy_from_slice(&random_bytes);
        seed_opts += 1;
    }

    if let Some(string) = matches.value_of("s") {
        eprintln!("Using supplied seed {}", string);
        seed_opts += 1;
        // pass supplied string through SHA1 to get actual seed
        // (less error checking required)
        starting_seed.
            copy_from_slice(
                sha1::Sha1::digest(string.as_bytes()).as_slice());
    }

    if seed_opts > 1 {
        eprintln!("The -n, -r and -s options are mutually exclusive");
        return;
    }
    
    // rng created below
    
    // code copied from equations.rs

    // coding side
    let mut message = sample_file(mblocks);
   
    // aux blocks stored at the end of message
    // let aux_blocks = Vec::with_capacity(ablocks);

    // check_blocks can be shared between coder and decoder
    let mut check_blocks = Vec::<u32>::with_capacity(200);

    // We need to know how many aux blocks to create, which is based
    // on q and e. However, we also have to sanity check and possibly
    // change the e value.  This code should be moved into one of the
    // modules somewhere.
    let mut f = max_degree(e);
    let mut ablocks = count_aux(mblocks, q, e);
    if f > mblocks + ablocks {
        eprintln!("Recalculating epsilon value");
        e = recalculate_e(mblocks, q, e);
        f = max_degree(e);
        ablocks = count_aux(mblocks, q, e);
        assert!(f <= mblocks + ablocks);
    }

    // We also need an RNG to create the auxiliary blocks.  In a
    // proper sender/receiver mode, the sender will advertise the seed
    // of the RNG used to generate this. With a large enough random
    // seed (160 bits is plenty), the sender could use a randomly
    // selected seed and prepend this to the packet header and use it
    // to uniquely identify the stream being sent.

    // aside on a possible packet format:
    //
    // +--------------+--------------+--------------------------+
    // | 1. stream id | 2. packet id | 3. payload (check block) |
    // +--------------+--------------+--------------------------+
    //
    // 1. Something that uniquely determines the online code
    // transmission stream. Besides that, it is also used as a seed
    // for pseudo-randomly selecting the auxiliary block mapping.
    //
    // 2. Sender randomly generates this for each check block that it
    // sends. Both sender and receiver then generate the same
    // selection of message/auxiliary blocks that comprise the check
    // block.
    //
    // 3. the check block value (xor of the blocks selected by the
    // seed in 2)
    //
    // (2 and 3 code for the lhs and rhs of a check block,
    // respectively)
    //
    // Alternatives:
    //
    // * send/advertise the seed for the stream via another channel
    //   and have server use a shorter unique stream id (which it also
    //   passes to receivers)
    //
    // * combine stream ID and packet (check block) ID when
    //   calculating lhs of check block equations (eg, turn check
    //   block seed into a sequence ID and calculate check block seed
    //   as hash(stream id + sequence base + sequence ID)
    //
    // * add checksum to protect against transmission errors (TCP and
    //   UDP packets/frames already have this though)

    // Rather than set up two distinct RNGs and have them generate the
    // auxiliary mapping twice, we can do it once and have both
    // encoder/decoder side share the generated mapping.

    let mut aux_rng = SHA1Rng::from_seed(starting_seed);
    let aux_map = AuxMapping::new(&mut aux_rng, mblocks, ablocks, q);

    // xor the aux blocks together based on the mapping and append
    // them to message for easier check block creation
    for aux_list in aux_map.aux_to_mblocks.iter() {
        let mut sum = 0u32;
        for mblock in aux_list {
            sum ^= message[*mblock];
        }
        message.push(sum);
    }

    // Further setup required: probability distribution table (which,
    // along with per-packet/per-check block rng) controls how many
    // message/auxiliary blocks each check block will contain.

    // The code this calls currently requires a partially-set up
    // CodeSettings structure, but a separate step is needed to set up
    // the probability distribution table.
    
    let p = Vec::<f64>::new();
    // TODO: I should also add q to the struct
    let mut settings = CodeSettings {
        e, f, mblocks,
        coblocks : mblocks + ablocks,
        p
    };

    // TODO: turn this into a method of CodeSettings?
    let new_p = init_rand_table(&settings).unwrap();
    settings.p = new_p;
    // eprintln!("p table is {:?}", settings.p);

    // decoding side
    let mut d = Decoder::new(mblocks, ablocks);
    d.add_aux_equations(& aux_map );

    // validate ordering of solutions (new solutions should only refer
    // to previous solutions)
    let mut solvedp = vec![false ; mblocks + ablocks];

    let mut done = false;
    let mut _pending = 0;
    let mut solved;

    while !done {

        // The real algorithm needs a fresh randomly-seeded RNG which
        // it uses for:
        //
        // * calling probdist::random_degree(), to find out how many
        //   blocks comprise this check block
        //
        // * picking that many blocks uniformly from message/auxiliary
        //   blocks (we use Floyd's algorithm)
        //
        // (sender has to send this seed to receivers)

        // I don't seem to have a support routine to generate a random
        // 20-byte seed, so I'll implement it here for now...
        let mut chk_rng = SHA1Rng::from_seed(rust_rng.gen::<[u8; 20]>());

        // use the probability distribution table 
        let picks = random_degree(&settings, &mut chk_rng);
        // eprintln!("picks is {}", picks);

        // I'm missing an equivalent to generate_check_block() that I
        // used in the unit test code, so I'll inline it here and will
        // refactor it out later.

        // Old code:
        // let check_vec = hashset_to_vec(
        //     &generate_check_block(picks, mblocks + ablocks));

        // Note: reuse chk_rng to make picks deterministic. This
        // doesn't matter here, but it would in sender/receiver setup
        let hashset = floyd_usize(&mut chk_rng, picks, mblocks + ablocks);
        // eprintln!("hashset = {:?}", hashset);
        let check_vec = hashset.iter().cloned().collect::<Vec<_>>();

        // eprintln!("check_vec = {:?}", check_vec);

        // end of changes (nothing after this depends on details of
        // settings struct or rngs)
        
        let mut check_val = 0;
        for index in check_vec.iter() {
            check_val ^= message[*index]
        }

        // eprintln!("Check block {} comprising: {:?}, value {}",
        //           check_blocks.len(), check_vec,  check_val);

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
            // eprintln!("Not storing a useless block");
            continue
        } else {
            check_blocks.push(check_val);
        }

        (done, _pending, solved) =
            d.add_check_equation(check_vec, false);

        if solved.is_none() {
            // see above for proper fix for "useless" check blocks
            // check_blocks.pop();
            continue
        }

        // solved was Some(value)
        let solved = solved.unwrap();

        for var in solved.iter() {

            let (chk, vars) = d.var_solution(*var).unwrap();

            // eprintln!("Checking solution for var {}: ({:?},{:?})",
            //           *var, chk, vars);

            // first part encodes an aux/check block solution
            let mut sum = match chk {
                None => 0,      // aux block => sum <- zero
                Some(x) => {    // chk block => sum <- check block
                    let value = check_blocks[x];
                    // eprintln!("Check {} value is {}", x, value);
                    value
                }
            };

            // the remaining values are variables to be xor'ed
            for v in vars.iter() {
                // make sure the variables were already solved
                assert_eq!(solvedp[*v], true);
                // that being the case, and because we check each
                // new var result (below), this is allowed: (in
                // practice, decoder will have its own structure)

                // eprintln!("adding message {} value {}",
                //           *v, message[*v]);
                sum ^= message[*v];
            }

            // eprintln!("Solving var {} ({} remain unsolved)",
            //           *var, d.count_unsolveds);

            // mark var as solved
            if solvedp[*var] {
                panic!("Var {} was already marked as solved", *var);
            } else {
                solvedp[*var] = true;
            }

            // compare result to original
            assert_eq!(sum, message[*var]);
        }

    }

    eprintln!("Successfully decoded after receiving {} check blocks",
              check_blocks.len());
}
