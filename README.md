# Common Lisp code collection for competitive programming
[![Build Status](https://travis-ci.com/privet-kitty/cl-competitive.svg?token=Tm5zQHEnGe2GCWmpu5C3&branch=master)](https://travis-ci.com/privet-kitty/cl-competitive)

## License
The greater part of this library is distributed as public domain, or licensed under either CC0 or the MIT license, whichever gives you the most rights in your legislation. Some code, however, has its specific license (usually because it is a dead copy of other library). For the details, please see the header of each file.

## Test environment
- latest SBCL (x64, linux)
- SBCL 1.3.3 (x64, linux) &mdash; CS Academy's version
- SBCL 1.2.6 (x64, linux) &mdash; the oldest SBCL that can be installed by roswell

Note that the version of SBCL is _1.1.14_ on AtCoder.

## Contents

### General data structures
- [generalized-bit.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/generalized-bit.lisp) binary indexed tree (aka Fenwick tree) on arbitrary commutative monoid
- [binary-indexed-tree.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/binary-indexed-tree.lisp) binary indexed tree (specialized for ordinary `+`)
- [2d-bit.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/2d-bit.lisp) 2D binary indexed tree
- [queue.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/queue.lisp) queue by singly-linked list
- [deque.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/deque.lisp) double-ended queue by ring buffer
- [disjoint-set.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/disjoint-set.lisp) disjoint set by Union-Find algorithm
- [persistent-disjoint-set.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/persistent-disjoint-set.lisp) partially persistent disjoint set by Union-Find algorithm
- [generalized-heap](https://github.com/privet-kitty/cl-competitive/blob/master/generalized-heap.lisp) binary heap for static order function
- [heap.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/heap.lisp) binary heap for dynamic order function
- [pairing-heap.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/pairing-heap.lisp) meldable heap (pairing heap)
- [ref-able-treap.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/ref-able-treap.lisp) ordered set by treap; analogue of `std::set` or `java.util.TreeSet`
- [explicit-treap.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/explicit-treap.lisp) ordered map by treap; analogue of `std::map` or `java.util.TreeMap`
- [implicit-treap.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/implicit-treap.lisp) treap with implicit key (so called Cartesian Tree)

### General algorithms
- [binsort.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/binsort.lisp) bin sort; counting sort
- [bisect.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/bisect.lisp) lower\_bound, upper\_bound
- [inversion-number.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/inversion-number.lisp) counting inversion number of vector by merge sort
- [map-permutations.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/map-permutations.lisp) permutation and combination
- [mo.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/mo.lisp) Mo's algorithm
- [power.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/power.lisp) exponentiation on arbitrary monoid
- [2dcumul.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/2dcumul.lisp) 2D cumulative sum
- [make-inverse-table.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/make-inverse-table.lisp) inverse lookup table of vector
- [adjacent-duplicates.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/adjacent-duplicates.lisp) deletion of adjacent duplicates; run-length encoding

### Arithmetic
- [modular-arithmetic.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/modular-arithmetic.lisp) extended Euclidean algorithm; Bezout equation; modular inverse; discrete logarithm; Gaussian elimination
- [power-mod.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/power-mod.lisp) modular exponentiation
- [binomial-coefficient-mod.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/binomial-coefficient-mod.lisp) binomial coefficient with fixed modulus; building tables of inverses, factorials, and inverses of factorials in O(n)
- [binomial-coefficient.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/binomial-coefficient.lisp) binomial coefficient by direct bignum arithmetic
- [partition-number.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/partition-number.lisp) partition number
- [bounded-partition-number.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/bounded-partition-number.lisp) partition number with upper-bound
- [dynamic-mod-operations.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/dynamic-mod-operations.lisp) addition/multiplication with dynamic modulus
- [mod-operations.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/mod-operations.lisp) addition/multiplication with static modulus
- [eratosthenes.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/eratosthenes.lisp) enumeration of primes; prime factorization
- [ext-eratosthenes.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/ext-eratosthenes.lisp) faster prime factorization than naive trial division
- [divisor.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/divisor.lisp) enumeration of divisors
- [enum-quotients.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/enum-quotients.lisp) enumeration of truncated quotients
- [f2.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/f2.lisp) linear algebra on GF(2)
- [walsh-hadamard.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/walsh-hadamard.lisp) Fast Walsh-Hadamard Transform

### Real and complex
- [fft.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/fft.lisp) complex FFT (radix-2)
- [fft-real.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/fft-real.lisp) real FFT (radix-2)
- [fft-recursive.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/fft-recursive.lisp) naive FFT by simple recursion
- [relative-error.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/relative-error.lisp) relative error
- [log-factorial.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/log-factorial.lisp) logarithm of factorial (logarithm of gamma function)

### Bit operations
- [bit-basher.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/bit-basher.lisp) 64-times faster operations on simple-bit-vector
- [gray-code.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/gray-code.lisp) Gray code
- [tzcount.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/tzcount.lisp) TZCNT operation
- [logreverse.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/logreverse.lisp) bit-reversal operation

### Graph
- [bipartite-matching.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/bipartite-matching.lisp) maximum bipartite matching
- [bipartite-p.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/bipartite-p.lisp) test of bipartiteness
- [bron-kerbosch.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/bron-kerbosch.lisp) maximum clique
- [ford-fulkerson.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/ford-fulkerson.lisp) maximum flow (Ford-Fulkerson algorithm)
- [dinic.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/dinic.lisp) maximum flow (Dinic's algorithm)
- [lca.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/lca.lisp) lowest common anscestor (binary lifting)
- [min-cost-flow.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/min-cost-flow.lisp) minimum cost flow
- [prim.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/prim.lisp) minimum spanning tree (Prim's algorithm)
- [topological-sort.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/topological-sort.lisp) topological sort on DAG
- [random-graph.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/random-graph.lisp) fast generation of random adjacency matrices

### Geometry
- [convex-hull.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/convex-hull.lisp) 2D convex hull (monotone chain algorithm)

### String algorithms
- [rolling-hash.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/rolling-hash.lisp) 32-bit rolling hash
- [rolling-hash62.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/rolling-hash62.lisp) 62-bit rolling hash
- [trie.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/trie.lisp) Trie

### I/O
- [buffered-read-line.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/buffered-read-line.lisp) [read-line-into.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/read-line-into.lisp) `read-line` into a given string
- [read-fixnum.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/read-fixnum.lisp) faster `read` for fixnum
- [read-bignum.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/read-bignum.lisp) faster `read` for bignum
- [with-buffered-stdout.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/with-buffered-stdout.lisp) buffering macro for `*standard-output*`

### Other utilities
- [with-cache.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/with-cache.lisp) memoization of function
- [dotimes-unroll.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/dotimes-unroll.lisp) loop unrolling

### Competition and struggle
- [template.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/template.lisp) template code
- [integer-pack.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/integer-pack.lisp) `defstruct`-like macro to use an integer as a bundle of some slots
- [increasing-stack-size.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/increase-stack-size.lisp) This header runs another SBCL as external process and leaves the entire processing to it. (This ugly hack was invented to increase the stack size of SBCL on contest sites.)
- [self-compile.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/self-compile.lisp) self-rewriting compilation
