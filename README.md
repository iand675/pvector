# pvector

Efficient persistent (immutable) vector library for Haskell, inspired by
Clojure's persistent vectors.

## Data Structures

### `Data.PVector.Back` (default, re-exported from `Data.PVector`)

A persistent vector based on a 32-way branching trie with a tail buffer.

- **snoc** (append): O(1) amortized
- **unsnoc** (remove last): O(1) amortized
- **index**: O(log₃₂ n) ≈ O(1) for practical sizes
- **update**: O(log₃₂ n)
- **head**: O(log₃₂ n)
- **last**: O(1)
- **fromList**: O(n)
- **foldl'/foldr**: O(n) with chunk-based traversal

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

- **Full typeclass instances**: `Foldable`, `Functor`, `Traversable`,
  `IsList`, `NFData`, `Eq`, `Ord`, `Show`, `Semigroup`, `Monoid`.

## Usage

```haskell
import qualified Data.PVector as V

-- Construction
let v = V.fromList [1..1000]
let v2 = V.snoc v 1001

-- Indexing
V.index v 500  -- 501
V.last v       -- 1000

-- Transformations
V.map (*2) v
V.filter even v

-- Transient (batch mutation)
let v3 = V.create $ \mv -> do
      mapM_ (V.mPush mv) [1..100]
      V.mWrite mv 50 999
```

## Benchmarks

Build and run benchmarks with:

```bash
cabal bench --enable-benchmarks
```

## Testing

```bash
cabal test --enable-tests
```

Property-based tests use Hedgehog.
