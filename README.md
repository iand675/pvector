# pvector

RRB-tree persistent (immutable) vector library for Haskell. Based on Relaxed
Radix Balanced trees (Stucki et al.), extending the Clojure/Scala model with
O(log n) concatenation and splitting.

## Data Structures

### `Data.PVector.Back` (default, re-exported from `Data.PVector`)

An RRB-tree persistent vector with symmetric prefix/suffix buffers.

Elements are stored in three regions:
1. **prefix** — leftmost partial leaf (prepend buffer, 0–31 elements)
2. **tree** — RRB-tree with balanced and relaxed nodes
3. **suffix/tail** — rightmost partial leaf (append buffer, 0–31 elements)

Relaxed nodes carry cumulative size tables (`PrimArray Int`) so indexing
can use binary search at relaxed levels, then fall through to radix
arithmetic at balanced levels below.

#### Complexity

- **cons** (prepend): O(eC) amortized — fills prefix buffer; flushes to tree every 32 elements
- **snoc** (append): O(eC) amortized — fills suffix buffer; flushes to tree every 32 elements
- **uncons** / **unsnoc**: O(eC) amortized
- **head**: O(1) — direct access to prefix buffer
- **last**: O(1) — direct access to suffix buffer
- **index**: O(log₃₂ n) — radix arithmetic at balanced nodes, binary search at relaxed nodes
- **update**: O(log₃₂ n) — persistent (old version unchanged)
- **concat / (++)**: O(log n) — RRB merge of right spine and left spine
- **take / drop / splitAt**: O(log n) — RRB split, creates relaxed nodes at boundaries
- **fromList**: O(n) — builds through transient
- **foldl' / foldr**: O(n) with direct tree walking
- **map**: O(n) — preserves trie structure

### `Data.PVector.Front`

A persistent vector with reversed internal ordering. `cons` maps to
`snoc` on the underlying vector.

### `Data.PVector.Deque`

A double-ended persistent vector (banker's deque) using two back vectors.

## Features

- **Transient (mutable) interface**: Convert to a mutable vector for batch
  operations, then freeze back to immutable. Uses copy-on-write for shared
  nodes, giving O(1) thaw.

- **Stream fusion**: GHC rewrite rules eliminate intermediate vectors in
  chains of operations like `map f . filter p . map g`.

- **Chunk-based operations**: `foldChunks`, `mapChunks`, `forChunks_`
  provide direct access to the 32-element `SmallArray` leaf nodes.

- **Full vector-compatible API**: Matches `Data.Vector`'s function names
  and signatures for easy migration. Includes `cons`, `snoc`, `init`,
  `tail`, `slice`, `splitAt`, `uncons`, `unsnoc`, `concatMap`, `mapMaybe`,
  `partition`, `span`, `break`, `find`, `findIndex`, `elem`, `all`/`any`,
  `sum`/`product`, `maximum`/`minimum`, `scanl`/`scanr`, `mapM`/`forM`,
  `enumFromTo`, `iterateN`, bulk update `(//)`, and more.

- **Typeclass instances**: `Functor`, `Foldable`, `Traversable`, `IsList`,
  `NFData`, `Eq`, `Ord`, `Show`, `Semigroup`, `Monoid`, `Applicative`,
  `Monad`, `MonadPlus`, `Alternative`.

### Why not `Data.Vector.Generic`?

`Data.Vector.Generic.Vector` requires O(1) `basicUnsafeSlice` on both
immutable and mutable vectors. RRB-trees provide O(log n) slicing —
much better than O(n) but still not O(1). We match the API surface
instead so code can migrate with minimal changes.

## Benchmarks (n = 10,000)

All benchmarks run on GHC 9.6 with `-O2`. Times are wall-clock means
from [Criterion](https://hackage.haskell.org/package/criterion).
Run `cabal bench` to reproduce.

### Core operations

| Operation | List | Vector | PVector | Seq |
|-----------|------|--------|---------|-----|
| snoc (build n) | 564 ms | 33.8 ms | **194 µs** | 152 µs |
| fromList | -- | 51.1 µs | 75.2 µs | 57.5 µs |
| index (middle) | 6.7 µs | 8.2 ns | 12.2 ns | 55.3 ns |
| head | 8.3 ns | 8.0 ns | 11.8 ns | 10.9 ns |
| last | 15.4 µs | 8.5 ns | **8.8 ns** | 10.8 ns |
| update (middle) | -- | 16.2 µs | 48.6 µs | 31.7 µs |
| foldl' (+) | 14.0 µs | 11.2 µs | **6.4 µs** | 27.4 µs |
| foldr (:) [] | 13.4 µs | 44.3 µs | 79.0 µs | 33.9 µs |
| map (+1) | 103 µs | 67.1 µs | 127 µs | 102 µs |
| filter even | 47.8 µs | 25.8 µs | 117 µs | 91.6 µs |
| reverse | 39.5 µs | 24.0 µs | 133 µs | 101 µs |
| take (n/2) | 24.8 µs | 4.6 µs | 103 µs | 15.9 µs |

**Highlights:**

- **snoc**: 174x faster than Vector, comparable to Seq — the key
  advantage of a persistent trie over a flat array.
- **index / head / last**: within 1.5x of Vector; `last` is O(1)
  since it reads directly from the tail buffer.
- **foldl'**: faster than Vector and List thanks to direct
  chunk-based tree walking with 32-element unrolled loops.
- **map / filter**: ~2x of Vector. The trie must be rebuilt node by
  node; Vector copies a flat array.
- **update**: 3x of Vector but O(log₃₂ n) — the old version is
  preserved (persistent).

### Charts

![snoc (build n)](doc/bench/snoc-build-n.svg)

![fromList](doc/bench/fromlist.svg)

![index (middle)](doc/bench/index-middle.svg)

![head](doc/bench/head.svg)

![last](doc/bench/last.svg)

![update (single, middle, nf)](doc/bench/update-single-middle-nf.svg)

![foldl' (+)](doc/bench/foldl.svg)

![foldr (:) \[\]](doc/bench/foldr.svg)

![map (+1)](doc/bench/map-1.svg)

![filter even](doc/bench/filter-even.svg)

![filter (> 0) keeps all](doc/bench/filter-0-keeps-all.svg)

![reverse](doc/bench/reverse.svg)

![take (n/2)](doc/bench/take-n-2.svg)

![append](doc/bench/append.svg)

## Usage

```haskell
import qualified Data.PVector as V

-- Construction
let v = V.fromList [1..1000]
let v2 = V.snoc v 1001       -- O(eC) append
let v3 = V.cons 0 v          -- O(eC) prepend (new!)

-- O(1) access to both ends
V.head v  -- 1
V.last v  -- 1000

-- O(log₃₂ n) indexing
V.index v 500  -- 501

-- O(log n) concatenation (new! was O(n))
let big = V.fromList [1..10000] <> V.fromList [10001..20000]

-- O(log n) slicing (new! was O(n))
let middle = V.take 500 (V.drop 250 v)  -- elements 251-750

-- Persistent update (old version preserved)
let v4 = V.update 500 42 v
V.index v  500  -- 501 (unchanged)
V.index v4 500  -- 42

-- Transformations
V.map (*2) v
V.filter even v

-- Transient (batch mutation)
let v5 = V.create $ \mv -> do
      mapM_ (V.mPush mv) [1..100]
      V.mWrite mv 50 999
```

## Building

```bash
cabal build
cabal test --enable-tests
cabal bench --enable-benchmarks
```
