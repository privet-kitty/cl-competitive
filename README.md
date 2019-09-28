# Common Lisp code collection for competitive programming
[![Build Status](https://travis-ci.com/privet-kitty/cl-competitive.svg?token=Tm5zQHEnGe2GCWmpu5C3&branch=master)](https://travis-ci.com/privet-kitty/cl-competitive)

This code collection is maintained mainly for competitive programming, and partly for just understanding algorithms.

## License
The greater part of this library is distributed as public domain, or licensed under either CC0 or the MIT license, whichever gives you the most rights in your legislation. Some code, however, has its specific license (usually because it is a dead copy of other library). For the details, please see the header of each file.

## Style
Currently I don't introduce any name spaces (packages) in each file. This is due to the circumstance unique to the competitive programming: one-file-per-submission. It is somewhat troublesome to manage many packages on a single file (especially when modifying inserted code). This style may change in the future, however.

On portability: I try not to abuse non-portable code though I sometimes resort to SBCL's extension and behaviour: e.g. declaration as assertion, bivalent stream, extensible sequence, `sb-kernel:%vector-raw-bits` and `sb-c:define-source-transform`. To my knowledge, every competition site adopts SBCL.

Every data structure and algorithm handles a 0-based index and a half-open interval unless otherwise noted.

## Test environment
- latest SBCL (x64, linux)
- SBCL 1.3.3 (x64, linux) &mdash; CS Academy's version
- SBCL 1.2.6 (x64, linux) &mdash; the oldest SBCL that can be installed by roswell

Note that the version of SBCL is _1.1.14_ on AtCoder.

## Contents

### General data structures
- [queue.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/queue.lisp) queue by singly-linked list
- [deque.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/deque.lisp) double-ended queue by ring buffer
- [abstract-heap](https://github.com/privet-kitty/cl-competitive/blob/master/abstract-heap.lisp) binary heap for static order function
- [heap.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/heap.lisp) binary heap for dynamic order function
- [pairing-heap.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/pairing-heap.lisp) meldable heap (pairing heap)
- [abstract-bit.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/abstract-bit.lisp) binary indexed tree (aka Fenwick tree) on arbitrary commutative monoid
- [binary-indexed-tree.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/binary-indexed-tree.lisp) binary indexed tree (specialized for ordinary `+`)
- [2d-bit.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/2d-bit.lisp) 2D binary indexed tree
- [disjoint-set.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/disjoint-set.lisp) disjoint set by Union-Find algorithm
- [persistent-disjoint-set.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/persistent-disjoint-set.lisp) partially persistent disjoint set by Union-Find algorithm
- [ref-able-treap.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/ref-able-treap.lisp) ordered set by treap; analogue of `std::set` or `java.util.TreeSet`
- [explicit-treap.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/explicit-treap.lisp) ordered map by treap; analogue of `std::map` or `java.util.TreeMap`
- [implicit-treap.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/implicit-treap.lisp) treap with implicit key
- [disjoint-sparse-table.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/disjoint-sparse-table.lisp) disjoint sparse table on arbitrary semigroup
- [range-tree.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/range-tree.lisp) 2D range tree on arbitrary commutative monoid

### General algorithms
- [bisect.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/bisect.lisp) analogue of `std::lower_bound` and `std::upper_bound`
- [binsort.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/binsort.lisp) bin sort; counting sort
- [map-permutations.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/map-permutations.lisp) permutation and combination
- [mo.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/mo.lisp) Mo's algorithm
- [make-inverse-table.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/make-inverse-table.lisp) inverse lookup table of vector
- [adjacent-duplicates.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/adjacent-duplicates.lisp) deletion of adjacent duplicates
- [map-run-length.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/map-run-length.lisp) run-length encoding
- [inversion-number.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/inversion-number.lisp) counting inversions of vector by merge sort
- [mex.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/mex.lisp) minimum excludant on non-negative integers
- [sliding-optimum.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/sliding-optimum.lisp) sliding window minimum (or maximum)

### Arithmetic and algebra
- [modular-arithmetic.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/modular-arithmetic.lisp) extended Euclidean algorithm; Bezout equation; modular inverse; discrete logarithm; Gaussian elimination
- [power-mod.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/power-mod.lisp) modular exponentiation
- [power.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/power.lisp) exponentiation on arbitrary monoid
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
- [gemm.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/gemm.lisp) matrix multiplication over semiring
- [f2.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/f2.lisp) linear algebra on GF(2)
- [walsh-hadamard.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/walsh-hadamard.lisp) fast Walsh-Hadamard transform
- [zeta-transform.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/zeta-transform.lisp) fast zeta/Möbius transform
- [zeta-integer.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/zeta-integer.lisp) fast zeta/Möbius transform w.r.t. divisors or multiples of integer

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
- [bipartite-matching.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/bipartite-matching.lisp) maximum bipartite matching (Ford-Fulkerson)
- [hopcroft-karp.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/hopcroft-karp.lisp) maximum bipartite matching (Hopcroft-Karp)
- [bipartite-p.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/bipartite-p.lisp) test of bipartiteness
- [bron-kerbosch.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/bron-kerbosch.lisp) maximum clique
- [ford-fulkerson.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/ford-fulkerson.lisp) maximum flow (Ford-Fulkerson algorithm)
- [dinic.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/dinic.lisp) maximum flow (Dinic's algorithm)
- [lca.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/lca.lisp) lowest common anscestor (binary lifting)
- [min-cost-flow.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/min-cost-flow.lisp) minimum cost flow
- [prim.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/prim.lisp) minimum spanning tree (Prim's algorithm)
- [topological-sort.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/topological-sort.lisp) topological sort on DAG
- [dfs-order.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/dfs-order.lisp) Euler tour of tree
- [condensation.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/condensation.lisp) strongly connected component of directed graph
- [block-cut-tree.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/block-cut-tree.lisp) biconnected component of undirected graph; block-cut tree
- [random-graph.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/random-graph.lisp) fast generation of random adjacency matrices

### Geometry
- [convex-hull.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/convex-hull.lisp) 2D convex hull (monotone chain algorithm)

### Pattern matching
- [rolling-hash.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/rolling-hash.lisp) 32-bit rolling hash
- [rolling-hash62.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/rolling-hash62.lisp) 62-bit rolling hash
- [2d-rolling-hash.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/2d-rolling-hash.lisp) 2D 32-bit rolling hash
- [trie.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/trie.lisp) trie
- [z-algorithm.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/z-algorithm.lisp) Z-algorithm

### I/O
- [read-line-into.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/read-line-into.lisp) `read-line` into a given string
- [buffered-read-line.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/buffered-read-line.lisp) `read-line` into a recycled string
- [read-fixnum.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/read-fixnum.lisp) faster `read` for fixnum
- [read-bignum.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/read-bignum.lisp) faster `read` for bignum
- [with-buffered-stdout.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/with-buffered-stdout.lisp) buffering macro for `*standard-output*`

### Other utilities
- [template.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/template.lisp) template code
- [with-cache.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/with-cache.lisp) memoization of function
- [dotimes-unroll.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/dotimes-unroll.lisp) loop unrolling
- [placeholder-syntax.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/placeholder-syntax.lisp) Clojure-style placeholder syntax

### Weird things
- [integer-pack.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/integer-pack.lisp) `defstruct`-like macro to deal with an integer as a bundle of some slots
- [increase-stack-size.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/increase-stack-size.lisp) This header runs another SBCL as external process and leaves the entire processing to it. (This ugly hack was invented to increase the stack size of SBCL on contest sites.)
- [self-compile.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/self-compile.lisp) self-rewriting compilation
