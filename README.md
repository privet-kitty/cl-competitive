# Common Lisp code collection for competitive programming
[![Build Status](https://github.com/privet-kitty/cl-competitive/workflows/CI/badge.svg)](https://github.com/privet-kitty/cl-competitive/actions)

This code collection is maintained mainly for competitive programming, and partly for just understanding algorithms.

## License
The greater part of this library is distributed as public domain, or licensed under either CC0 or the MIT license, whichever gives you the most rights in your legislation. Some code, however, has its specific license (usually because it is a dead copy of other library). For the details, please see the header of each file.

## Style
Although I put this code collection in a ASDF module, this project is primarily made for competitive programming and the whole structure will be quite different from a common ASDF library. I don't recommend that you directly load this module for your software.

On portability: I try to write portable code as long as the code structure doesn't get too complicated. However, I occasionally use extensions and behaviours of SBCL x86-64, mainly for performance reasons. Below is a SBCL-specific feature that I always depend on:

- declaration as assertion

Below are the features that I sometimes use:

- bivalent stream
- extensible sequence
- `sb-ext:array-storage-vector` and `sb-kernel:%vector-raw-bits`
- `sb-c:define-source-transform`

Every data structure and algorithm uses a 0-based index and a half-open interval unless otherwise noted.

## Test environment
- SBCL 2.3.6 (x64, linux) &mdash; AtCoder's version
- SBCL 2.1.6 (x64, linux) &mdash; yukicoder's version
- SBCL 1.3.13 (x64, linux) &mdash; CodeChef's version
- SBCL 1.3.1 (x64, linux) &mdash; HackerEarth's version


## Contents

### Unclassified data structures
- [queue.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/queue.lisp) queue with singly-linked list
- [deque.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/deque.lisp) double-ended queue with ring buffer
- [double-stack-deque.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/double-stack-deque.lisp) double-ended queue with two stacks
- [binary-heap](https://github.com/privet-kitty/cl-competitive/blob/master/module/binary-heap.lisp) binary heap for static or dynamic order function
- [pairing-heap.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/pairing-heap.lisp) meldable heap (pairing heap)
- [radix-heap.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/radix-heap.lisp) radix heap
- [segment-tree.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/segment-tree.lisp) segment tree over an arbitrary monoid
- [simple-dual-segment-tree.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/simple-dual-segment-tree.lisp) dual segment tree for a commutative operator
- [persistent-segment-tree.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/persistent-segment-tree.lisp) persistent segment tree
- [persistent-starry-sky-tree.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/persistent-starry-sky-tree.lisp) persistent starry sky tree
- [binary-indexed-tree.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/binary-indexed-tree.lisp) binary indexed tree (aka Fenwick tree) over an arbitrary commutative monoid
- [biset.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/biset.lisp) binary indexed tree for binary data
- [2d-bit.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/2d-bit.lisp) 2D binary indexed tree
- [disjoint-set.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/disjoint-set.lisp) disjoint set by Union-Find algorithm
- [undoable-disjoint-set.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/undoable-disjoint-set.lisp) undoable disjoint set
- [persistent-disjoint-set.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/persistent-disjoint-set.lisp) partially persistent disjoint set
- [offline-dynamic-connectivity.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/offline-dynamic-connectivity.lisp) offline dynamic connectivity
- [ref-able-treap.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/ref-able-treap.lisp) ordered set with treap; analogue of `std::set` or `java.util.TreeSet`
- [explicit-treap.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/explicit-treap.lisp) ordered map with treap; analogue of `std::map` or `java.util.TreeMap`
- [implicit-treap.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/implicit-treap.lisp) treap with implicit key
- [multiset.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/multiset.lisp) multiset
- [interval-set.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/interval-set.lisp) ordered set of half-open intervals
- [disjoint-sparse-table.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/disjoint-sparse-table.lisp) disjoint sparse table on arbitrary semigroup
- [range-tree.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/range-tree.lisp) 2D range tree over an arbitrary commutative monoid
- [range-tree-fc.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/range-tree-fc.lisp) 2D range tree with fractional cascading
- [convex-hull-trick.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/convex-hull-trick.lisp) convex hull trick
- [succinct-bit-vector.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/succinct-bit-vector.lisp) three-layer succinct bit vector
- [compact-bit-vector.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/compact-bit-vector.lisp) two-layer compact bit vector
- [wavelet-matrix.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/wavelet-matrix.lisp) wavelet matrix
- [persistent-vector.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/persistent-vector.lisp) persistent vector
- [dice.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/dice.lisp) six-sided dice
- [swag.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/swag.lisp) sliding window aggregation
- [sliding-window.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/sliding-window.lisp) sliding window extremum

### Unclassified algorithms
- [cumulative-sum.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/cumulative-sum.lisp) n-dimensional cumulative sum
- [bisect.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/bisect.lisp) analogue of `std::lower_bound` and `std::upper_bound`
- [trisect.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/trisect.lisp) extremum of strictly unimodal function
- [monotone-minima.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/monotone-minima.lisp) divide-and-conquer algorithm for monotone matrix
- [quicksort.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/quicksort.lisp) quicksort
- [merge-sort.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/merge-sort.lisp) merge sort
- [inplace-merge-sort.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/inplace-merge.lisp) in-place merge sort
- [binsort.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/binsort.lisp) bin sort; counting sort
- [map-permutations.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/map-permutations.lisp) permutation and combination
- [next-permutation.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/next-permutation.lisp) next permutation w.r.t. lexicographical order
- [inversion-sequence.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/inversion-sequence.lisp) inversion sequence of permutation
- [mo.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/mo.lisp) Mo's algorithm
- [inverse-table.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/inverse-table.lisp) inverse lookup table of vector
- [adjacent-duplicates.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/adjacent-duplicates.lisp) deletion of adjacent duplicates
- [run-length.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/run-length.lisp) run-length encoding
- [inversion-number.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/inversion-number.lisp) counting inversions of vector by merge sort
- [lis.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/lis.lisp) longest increasing subsequence
- [mex.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/mex.lisp) minimum excludant on non-negative integers
- [quickselect.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/quickselect.lisp) expected O(n) algorithm for k-th order statistic of sequence
- [symmetric-group.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/symmetric-group.lisp) decomposition to cyclic permutations and some operations on a symmetric group
- [fkm.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/fkm.lisp) Fredricksen, Kessler, and Maiorana algorithm
- [manhattan-fnn.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/manhattan-fnn.lisp) farthest neighbor points in d-dimensional L1 space
- [round-robin.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/round-robin.lisp) scheduling algorithm for a round robin tournament (circle method)
- [hackenbush.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/hackenbush.lisp) game value of Hackenbush
- [date.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/date.lisp) some utilities about date


### Arithmetic and algebra

- [mod-power.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/mod-power.lisp) modular exponentiation
- [ext-gcd.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/ext-gcd.lisp) extended euclidean algorithm
- [mod-inverse.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/mod-inverse.lisp) modular inverse
- [mod-log.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/mod-log.lisp) modular logarithm
- [bsgs.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/bsgs.lisp) baby-step giant-step algorithm for discrete logarithm problem on (semi)group
- [bezout.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/bezout.lisp) Bezout equation
- [mod-linear-algebra.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/mod-linear-algebra.lisp) modular linear algebra
- [power.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/power.lisp) exponentiation over an arbitrary monoid
- [binom-mod-prime.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/binom-mod-prime.lisp) binomial coefficient modulo prime; linear-time construction of tables of inverses, factorials, and inverse of factorials
- [binom-mod-small.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/binom-mod-small.lisp) binomial coefficient modulo small number
- [partition-number.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/partition-number.lisp) partition number
- [bounded-partition-number.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/bounded-partition-number.lisp) partition number with upper-bound
- [perfect-kth-powers.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/perfect-kth-powers.lisp) sequence of perfect powers
- [eulerian-polynomial.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/eulerian-polynomial.lisp) Eulerian polynomial
- [mod-operations.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/mod-operations.lisp) modular arithmetic
- [lagrange-interpolation.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/lagrange-interpolation.lisp) Lagrange interpolation
- [eratosthenes.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/eratosthenes.lisp) enumeration of primes; prime factorization
- [linear-sieve.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/linear-sieve.lisp) linear sieve; fast prime factorization
- [divisor.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/divisor.lisp) enumeration of divisors of a given number
- [divisor-table.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/divisor-table.lisp) enumeration of divisors of first n natural numbers
- [primality.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/primality.lisp) primality test (deterministic Miller-Rabin)
- [integer-root](https://github.com/privet-kitty/cl-competitive/blob/master/module/integer-root.lisp) integer nth root
- [gemm.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/gemm.lisp) matrix multiplication over semiring
- [freiwald.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/freiwald.lisp) Freiwalds' algorithm
- [f2.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/f2.lisp) linear algebra on GF(2)
- [walsh-hadamard.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/walsh-hadamard.lisp) fast Walsh-Hadamard transform
- [zeta-transform.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/zeta-transform.lisp) fast zeta/Möbius transform
- [zeta-integer.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/zeta-integer.lisp) fast zeta/Möbius transform w.r.t. divisors or multiples of integer
- [farey.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/farey.lisp) iteration on Farey sequence
- [farey-next.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/farey-next.lisp) next/previous element on Farey sequence
- [hermite-normal-form.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/hermite-normal-form.lisp) Hermite normal form
- [lll.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/lll.lisp) LLL algorithm for basis reduction

### Real and complex
- [fft.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/fft.lisp) complex FFT (radix-2)
- [fft-real.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/fft-real.lisp) real FFT (radix-2)
- [fft-recursive.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/fft-recursive.lisp) naive FFT by simple recursion
- [relative-error.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/relative-error.lisp) relative error
- [log-factorial.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/log-factorial.lisp) logarithm of factorial (logarithm of gamma function)

### Bit operations
- [bit-basher.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/bit-basher.lisp) efficient operations on simple-bit-vector
- [gray-code.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/gray-code.lisp) Gray code
- [tzcount.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/tzcount.lisp) TZCNT operation
- [logreverse.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/logreverse.lisp) bit-reversal operation

### Graph
- [bipartite-matching.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/bipartite-matching.lisp) maximum bipartite matching (Ford-Fulkerson)
- [hopcroft-karp.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/hopcroft-karp.lisp) maximum bipartite matching (Hopcroft-Karp)
- [jonker-volgenant.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/jonker-volgenant.lisp) weighted bipartite matching (Jonker-Volgenant)
- [gabow-edmonds.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/jonker-volgenant.lisp) maximum non-bipartite matching (Gabow-Edmonds)
- [bipartite-p.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/bipartite-p.lisp) test of bipartiteness
- [bron-kerbosch.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/bron-kerbosch.lisp) maximum clique
- [ford-fulkerson.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/ford-fulkerson.lisp) maximum flow (Ford-Fulkerson algorithm)
- [dinic.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/dinic.lisp) maximum flow (Dinic's algorithm)
- [lca.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/lca.lisp) lowest common anscestor (binary lifting)
- [binary-lifting](https://github.com/privet-kitty/cl-competitive/blob/master/module/binary-lifting.lisp) LCA and path queries on a static tree or forest with weighted edge or vertex (binary lifting)
- [ssp.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/ssp.lisp) minimum cost flow (SSP)
- [topological-sort.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/topological-sort.lisp) topological sort on DAG
- [find-cycle.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/find-cycle.lisp) (explicit) cycle detection
- [euler-tour.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/euler-tour.lisp) Euler tour of tree
- [scc.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/scc.lisp) strongly connected component of directed graph (Tarjan's algorithm)
- [2sat.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/2sat.lisp) 2SAT solver
- [2cc.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/2cc.lisp) two-edge connected component of undirected graph
- [tree-centroid.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/tree-centroid.lisp) centroid decomposition of tree
- [chordal-graph.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/chordal-graph.lisp) recognition of graph chordality (maximum cardinality search); perfect elimination order
- [diameter.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/diameter.lisp) diameter of tree
- [tree-hash.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/tree-hash.lisp) hashing of rooted tree
- [mo-tree.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/mo-tree.lisp) Mo's algorithm for paths on tree (vertex query)
- [random-graph.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/random-graph.lisp) fast generation of random adjacency matrices

### Optimization

- [two-phase-simplex.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/two-phase-simplex.lisp) two-phase (dual-then-primal) simplex method for dense LP
- [self-dual-simplex.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/self-dual-simplex.lisp) parametric self-dual simplex method for dense LP
- [incremental-lp.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/incremental-lp.lisp) warm-start LP solver for dynamically added constraints, using dual simplex method for dense LP
- [sparse-simplex.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/sparse-simplex.lisp) two-phase (dual-then-primal) simplex method and self-dual simplex method for sparse LP
- [ols.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/ols.lisp) ordinary least squares regression by Gaussian elimination

### Euclidean geometry
- [complex-geometry.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/complex-geometry.lisp) some utilities for 2D geometry with complex number
- [phase.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/phase.lisp) order by amplitude (`atan`)
- [circumcenter.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/circumcenter.lisp) circumcenter
- [welzl.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/welzl.lisp) smallest circle problem (Welzl's algorithm)
- [convex-hull.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/convex-hull.lisp) 2D convex hull (monotone chain algorithm)

### String
- [rolling-hash31.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/rolling-hash31.lisp) 31-bit rolling hash
- [rolling-hash62.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/rolling-hash62.lisp) 62-bit rolling hash
- [2d-rolling-hash.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/2d-rolling-hash.lisp) 2D 32-bit rolling hash
- [triemap.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/triemap.lisp) map structure with trie
- [trie.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/trie.lisp) multiset structure with trie
- [z-algorithm.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/z-algorithm.lisp) Z-algorithm

### I/O
- [read-line-into.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/read-line-into.lisp) `read-line` into a given string
- [buffered-read-line.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/buffered-read-line.lisp) `read-line` into a recycled string
- [read-fixnum.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/read-fixnum.lisp) faster `read` for fixnum
- [read-bignum.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/read-bignum.lisp) faster `read` for bignum
- [write-double-float.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/write-double-float.lisp) write double-float with fixed-point expression

### Other utilities
- [template.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/non-module/template.lisp) template code
- [mpfr.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/non-module/mpfr.lisp) header to load SB-MPFR
- [with-cache.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/with-cache.lisp) memoization of function
- [dotimes-unroll.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/dotimes-unroll.lisp) loop unrolling
- [placeholder-syntax.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/placeholder-syntax.lisp) Clojure-style placeholder syntax

### Weird things
- [integer-pack.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/module/integer-pack.lisp) `defstruct`-like macro to deal with an integer as a structure of several integer slots
- [increase-space.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/non-module/increase-space.lisp) This header runs another SBCL as external process and leaves the entire processing to it. (This ugly hack was invented to increase the stack size of SBCL on online judges.)
- [compile-time-increase-space.lisp](https://github.com/privet-kitty/cl-competitive/blob/master/non-module/compile-time-increase-space.lisp) analogue of increase-space at compile time
