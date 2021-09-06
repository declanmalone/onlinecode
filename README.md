# An implementation of "Online Codes", a type of Fountain Code

This is just a placeholder package at the moment. It is intended to
become a port/rework of my existing Perl/C implementations.

The existing Perl implementation is available as
[Net::OnlineCode](https://metacpan.org/pod/Net::OnlineCode) on
CPAN. The source is available [on
github](https://github.com/declanmalone/gnetraid) under the
`Net-OnlineCode` directory.

I note that there is another similarly-named package here on
`crates.io` which implements the same algorithm. If you came here
expecting to find that, you can find it
[here](https://crates.io/crates/online_codes).

## Version 0.1.1

This version implements a simple codec (encoder/decoder) that tests
that all the code in the various modules works correctly.

Points of interest/note:

* uses more natural idea of solving equations involving variables
  instead of implementing explicit bipartite graph

* takes an RNG that implements `RngCore` rather than a fixed RNG
  (allows for easy customisation)

* codec doesn't work with files (instead, one block = one u32), but
  that's easily extended

* library only handles Online Code algorithm, so user is responsible
  for XORing file blocks (and networking)

* use of HashSet turns out to be a big problem as far as performance
  is concerned; replacing it with even an unsorted list would probably
  improve things a lot. HashSet is also a source of non-determinism
  due to iterators returning set members in a random order.

* is a faithful implementation of the original paper

* core routines are well tested and I have a high degree of confidence
  that they're correct



