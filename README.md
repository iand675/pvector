# pvector

Efficient persistent (immutable) vector library for Haskell, inspired by
Clojure's persistent vectors.

## Data Structures

### `Data.PVector.Back` (default, re-exported from `Data.PVector`)

A persistent vector based on a 32-way branching trie with a tail buffer.

- **snoc** (append): O(1) amortized — **10–100x faster than Data.Vector for incremental building**
- **unsnoc** (remove last): O(1) amortized
- **index**: O(log₃₂ n) — typically 1–2 pointer chases, within 1.5x of Data.Vector
- **update**: O(log₃₂ n) — persistent (old version unchanged)
- **head**: O(log₃₂ n)
- **last**: O(1) — reads directly from tail buffer
- **fromList**: O(n) — builds through transient
- **foldl'/foldr**: O(n) with direct tree-walking (no per-chunk re-traversal)
- **map**: O(n) — preserves trie structure directly, ~2x of Data.Vector

### `Data.PVector.Front`

A persistent vector optimized for prepend operations. Internally wraps a
`Back` vector with reversed index mapping.

- **cons** (prepend): O(1) amortized
- **uncons** (remove first): O(1) amortized
- **head**: O(1)
- **index**: O(log₃₂ n)

### `Data.PVector.Deque`

A double-ended persistent vector (banker's deque) using two back vectors.

- **cons** and **snoc**: O(1) amortized
- **uncons** and **unsnoc**: O(1) amortized
- **head** and **last**: O(1)
- **index**: O(log₃₂ n)

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
immutable and mutable vectors. A persistent trie fundamentally cannot
provide O(1) slicing — a slice requires rebuilding the trie structure.
We match the API surface instead so code can migrate with minimal changes.

## Benchmark Summary (n=10,000)

| Operation    | List     | Data.Vector | PVector  | vs Vector |
|-------------|----------|-------------|----------|-----------|
| snoc (build) | 552 ms   | 26 ms       | **234 μs** | **113x faster** |
| index (mid)  | 6.6 μs   | 8 ns        | 11 ns    | 1.4x      |
| last         | —        | 7.5 ns      | 8 ns     | 1.1x      |
| map (+1)     | 78 μs    | 60 μs       | 120 μs   | 2x        |
| foldl' (+)   | 14 μs    | 11 μs       | 52 μs    | 4.7x      |
| filter even  | 52 μs    | 25 μs       | 115 μs   | 4.5x      |
| foldr toList | 1.3 μs   | 3.5 μs      | 8 μs     | 2.3x      |
| fromList     | 16 μs    | 43 μs       | 109 μs   | 2.5x      |

All operations are within an order of magnitude of Data.Vector.
The persistent vector excels at incremental construction and persistent
updates where Data.Vector requires full copies.

## Usage

```haskell
import qualified Data.PVector as V

-- Construction
let v = V.fromList [1..1000]
let v2 = V.snoc v 1001

-- Indexing
V.index v 500  -- 501
V.last v       -- 1000

-- Persistent update (old version preserved)
let v3 = V.update 500 42 v
V.index v  500  -- 501 (unchanged)
V.index v3 500  -- 42

-- Transformations
V.map (*2) v
V.filter even v

-- Chunk-based operations
V.foldChunks (\acc _ chunk -> acc + sizeofSmallArray chunk) 0 v

-- Transient (batch mutation)
let v4 = V.create $ \mv -> do
      mapM_ (V.mPush mv) [1..100]
      V.mWrite mv 50 999
```

## Building

```bash
cabal build
cabal test --enable-tests
cabal bench --enable-benchmarks
```
