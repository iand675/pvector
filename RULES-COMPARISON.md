# GHC Rewrite Rules Comparison: vector/vector-stream vs pvector

## 1. All Rules in vector-stream (Data/Stream/Monadic.hs)

| # | Rule Name | Line | Description |
|---|-----------|------|-------------|
| 1 | `"enumFromTo<Int8> [Stream]"` | L1390 | Specialise `enumFromTo` to `enumFromTo_small` for `Int8` |
| 2 | `"enumFromTo<Int16> [Stream]"` | L1393 | Specialise `enumFromTo` to `enumFromTo_small` for `Int16` |
| 3 | `"enumFromTo<Word8> [Stream]"` | L1396 | Specialise `enumFromTo` to `enumFromTo_small` for `Word8` |
| 4 | `"enumFromTo<Word16> [Stream]"` | L1399 | Specialise `enumFromTo` to `enumFromTo_small` for `Word16` |
| 5 | `"enumFromTo<Int32> [Stream]"` | L1407 | Specialise `enumFromTo` to `enumFromTo_small` for `Int32` (32-bit only) |
| 6 | `"enumFromTo<Word32> [Stream]"` | L1410 | Specialise `enumFromTo` to `enumFromTo_small` for `Word32` (32-bit only) |
| 7 | `"enumFromTo<Int> [Stream]"` | L1460 | Specialise `enumFromTo` to `enumFromTo_int` for `Int` |
| 8 | `"enumFromTo<Int64> [Stream]"` | L1465 | Specialise `enumFromTo` to `enumFromTo_intlike` for `Int64` (64-bit) |
| 9 | `"enumFromTo<Int32> [Stream]"` | L1470 | Specialise `enumFromTo` to `enumFromTo_intlike` for `Int32` (32-bit only, alternate) |
| 10 | `"enumFromTo<Word> [Stream]"` | L1487 | Specialise `enumFromTo` to `enumFromTo_big_word` for `Word` |
| 11 | `"enumFromTo<Word64> [Stream]"` | L1490 | Specialise `enumFromTo` to `enumFromTo_big_word` for `Word64` |
| 12 | `"enumFromTo<Word32> [Stream]"` | L1496 | Specialise `enumFromTo` to `enumFromTo_big_word` for `Word32` (32-bit only) |
| 13 | `"enumFromTo<Integer> [Stream]"` | L1502 | Specialise `enumFromTo` to `enumFromTo_big_word` for `Integer` |
| 14 | `"enumFromTo<Int64> [Stream]"` | L1523 | Specialise `enumFromTo` to `enumFromTo_big_int` for `Int64` (32-bit only) |
| 15 | `"enumFromTo<Char> [Stream]"` | L1543 | Specialise `enumFromTo` to `enumFromTo_char` for `Char` |
| 16 | `"enumFromTo<Double> [Stream]"` | L1579 | Specialise `enumFromTo` to `enumFromTo_double` for `Double` |
| 17 | `"enumFromTo<Float> [Stream]"` | L1582 | Specialise `enumFromTo` to `enumFromTo_double` for `Float` |
| 18 | `"reVector [Vector]"` | L1708 | `reVector = id` (identity on re-tagging) |
| 19 | `"reVector/reVector [Vector]"` | L1711 | `reVector (reVector s) = s` (idempotent) |

**Note:** Rules 18-19 appear in a commented-out block (`{- ... -}`) at the end of the file. They are legacy/dead code.

---

## 2. All Rules in vector (Data/Vector/Fusion/Bundle/Monadic.hs)

| # | Rule Name | Line | Description |
|---|-----------|------|-------------|
| 1 | `"zipWithM xs xs [Vector.Bundle]"` | L345 | `zipWithM f (lift xs) (lift xs) = mapM (\x -> f x x) (lift xs)` — self-zip optimisation |
| 2 | `"enumFromTo<Int8> [Bundle]"` | L815 | Specialise `enumFromTo` to `enumFromTo_small` for `Int8` |
| 3 | `"enumFromTo<Int16> [Bundle]"` | L818 | Specialise `enumFromTo` to `enumFromTo_small` for `Int16` |
| 4 | `"enumFromTo<Word8> [Bundle]"` | L821 | Specialise `enumFromTo` to `enumFromTo_small` for `Word8` |
| 5 | `"enumFromTo<Word16> [Bundle]"` | L824 | Specialise `enumFromTo` to `enumFromTo_small` for `Word16` |
| 6 | `"enumFromTo<Int32> [Bundle]"` | L833 | Specialise `enumFromTo` to `enumFromTo_small` for `Int32` |
| 7 | `"enumFromTo<Word32> [Bundle]"` | L836 | Specialise `enumFromTo` to `enumFromTo_small` for `Word32` |
| 8 | `"enumFromTo<Int> [Bundle]"` | L891 | Specialise `enumFromTo` to `enumFromTo_int` for `Int` |
| 9 | `"enumFromTo<Int64> [Bundle]"` | L896 | Specialise `enumFromTo` to `enumFromTo_intlike` for `Int64` |
| 10 | `"enumFromTo<Word> [Bundle]"` | L929 | Specialise `enumFromTo` to `enumFromTo_big_word` for `Word` |
| 11 | `"enumFromTo<Word64> [Bundle]"` | L932 | Specialise `enumFromTo` to `enumFromTo_big_word` for `Word64` |
| 12 | `"enumFromTo<Word32> [Bundle]"` | L938 | Specialise `enumFromTo` to `enumFromTo_big_word` for `Word32` (conditional) |
| 13 | `"enumFromTo<Integer> [Bundle]"` | L944 | Specialise `enumFromTo` to `enumFromTo_big_word` for `Integer` |
| 14 | `"enumFromTo<Int64> [Bundle]"` | L974 | Specialise `enumFromTo` to `enumFromTo_big_int` for `Int64` (conditional) |
| 15 | `"enumFromTo<Char> [Bundle]"` | L996 | Specialise `enumFromTo` to `enumFromTo_char` for `Char` |
| 16 | `"enumFromTo<Double> [Bundle]"` | L1041 | Specialise `enumFromTo` to `enumFromTo_double` for `Double` |
| 17 | `"enumFromTo<Float> [Bundle]"` | L1044 | Specialise `enumFromTo` to `enumFromTo_double` for `Float` |
| 18 | `"reVector [Vector]"` | L1170 | `reVector = id` |
| 19 | `"reVector/reVector [Vector]"` | L1173 | `reVector (reVector s) = s` |

---

## 3. All Rules in vector (Data/Vector/Fusion/Bundle.hs)

| # | Rule Name | Line | Description |
|---|-----------|------|-------------|
| 1 | `"inplace/inplace [Vector]"` | L117 | `inplace f1 g1 (inplace f2 g2 s) = inplace (f1 . f2) (g1 . g2) s` — compose adjacent inplace transforms |

---

## 4. All Rules in vector (Data/Vector/Generic.hs)

### 4a. Consumer forwarding rules (indexing through unstream)

| # | Rule Name | Line | Description |
|---|-----------|------|-------------|
| 1 | `"(!)/unstream [Vector]"` | L290 | `new (New.unstream s) ! i = s Bundle.!! i` — index directly from bundle |
| 2 | `"(!?)/unstream [Vector]"` | L293 | `new (New.unstream s) !? i = s Bundle.!? i` — safe index directly from bundle |
| 3 | `"head/unstream [Vector]"` | L296 | `head (new (New.unstream s)) = Bundle.head s` |
| 4 | `"last/unstream [Vector]"` | L299 | `last (new (New.unstream s)) = Bundle.last s` |
| 5 | `"unsafeIndex/unstream [Vector]"` | L302 | `unsafeIndex (new (New.unstream s)) i = s Bundle.!! i` |
| 6 | `"unsafeHead/unstream [Vector]"` | L305 | `unsafeHead (new (New.unstream s)) = Bundle.head s` |
| 7 | `"unsafeLast/unstream [Vector]"` | L308 | `unsafeLast (new (New.unstream s)) = Bundle.last s` |

### 4b. Monadic consumer forwarding rules

| # | Rule Name | Line | Description |
|---|-----------|------|-------------|
| 8 | `"indexM/unstream [Vector]"` | L372 | `indexM (new (New.unstream s)) i = lift s MBundle.!! i` |
| 9 | `"headM/unstream [Vector]"` | L375 | `headM (new (New.unstream s)) = MBundle.head (lift s)` |
| 10 | `"lastM/unstream [Vector]"` | L378 | `lastM (new (New.unstream s)) = MBundle.last (lift s)` |
| 11 | `"unsafeIndexM/unstream [Vector]"` | L381 | `unsafeIndexM (new (New.unstream s)) i = lift s MBundle.!! i` |
| 12 | `"unsafeHeadM/unstream [Vector]"` | L384 | `unsafeHeadM (new (New.unstream s)) = MBundle.head (lift s)` |
| 13 | `"unsafeLastM/unstream [Vector]"` | L387 | `unsafeLastM (new (New.unstream s)) = MBundle.last (lift s)` |

### 4c. Slicing through `new` (vector recycling)

| # | Rule Name | Line | Description |
|---|-----------|------|-------------|
| 14 | `"init/new [Vector]"` | L506 | `init (new p) = new (New.init p)` — push init inside New |
| 15 | `"tail/new [Vector]"` | L509 | `tail (new p) = new (New.tail p)` |
| 16 | `"take/new [Vector]"` | L512 | `take n (new p) = new (New.take n p)` |
| 17 | `"drop/new [Vector]"` | L515 | `drop n (new p) = new (New.drop n p)` |
| 18 | `"unsafeSlice/new [Vector]"` | L518 | `unsafeSlice i n (new p) = new (New.unsafeSlice i n p)` |
| 19 | `"unsafeInit/new [Vector]"` | L521 | `unsafeInit (new p) = new (New.unsafeInit p)` |
| 20 | `"unsafeTail/new [Vector]"` | L524 | `unsafeTail (new p) = new (New.unsafeTail p)` |

### 4d. Miscellaneous fusion rules

| # | Rule Name | Line | Description |
|---|-----------|------|-------------|
| 21 | `"dropWhile/unstream [Vector]"` | L1472 | `dropWhile f (new (New.unstream p)) = new (New.unstream (Bundle.dropWhile f p))` — fuse dropWhile into bundle |
| 22 | `"unstablePartition"` | L1546 | `unstablePartition_stream f (stream (new p)) = unstablePartition_new f p` — avoid materialising intermediate vector |

### 4e. Thaw rules

| # | Rule Name | Line | Description |
|---|-----------|------|-------------|
| 23 | `"unsafeThaw/new [Vector]"` | L2463 | `unsafeThaw (new p) = New.runPrim p` — avoid freeze/thaw roundtrip |
| 24 | `"thaw/new [Vector]"` | L2466 | `thaw (new p) = New.runPrim p` |

### 4f. Core stream/unstream fusion

| # | Rule Name | Line | Description |
|---|-----------|------|-------------|
| 25 | `"stream/unstream [Vector]"` | L2535 | `stream (new (New.unstream s)) = s` — fundamental stream fusion identity |
| 26 | `"New.unstream/stream [Vector]"` | L2538 | `New.unstream (stream v) = clone v` — clone shortcut |
| 27 | `"clone/new [Vector]"` | L2541 | `clone (new p) = p` — eliminate clone of freshly-built vector |
| 28 | `"inplace [Vector]"` | L2544 | `New.unstream (inplace f g (stream (new m))) = New.transform f g m` — fold inplace into New transform |
| 29 | `"uninplace [Vector]"` | L2548 | `stream (new (New.transform f g m)) = inplace f g (stream (new m))` — expose inplace form for further fusion |

### 4g. Right-to-left (reverse) stream fusion

| # | Rule Name | Line | Description |
|---|-----------|------|-------------|
| 30 | `"streamR/unstreamR [Vector]"` | L2574 | `streamR (new (New.unstreamR s)) = s` — right-to-left stream fusion identity |
| 31 | `"New.unstreamR/streamR/new [Vector]"` | L2577 | `New.unstreamR (streamR (new p)) = p` — right-to-left recycling |
| 32 | `"New.unstream/streamR/new [Vector]"` | L2580 | `New.unstream (streamR (new p)) = New.modify M.reverse p` — cross-direction: build reversed |
| 33 | `"New.unstreamR/stream/new [Vector]"` | L2583 | `New.unstreamR (stream (new p)) = New.modify M.reverse p` — cross-direction: build reversed |
| 34 | `"inplace right [Vector]"` | L2586 | `New.unstreamR (inplace f g (streamR (new m))) = New.transformR f g m` |
| 35 | `"uninplace right [Vector]"` | L2590 | `streamR (new (New.transformR f g m)) = inplace f g (streamR (new m))` |

### 4h. Monadic unstream specialisation

| # | Rule Name | Line | Description |
|---|-----------|------|-------------|
| 36 | `"unstreamM[IO]"` | L2620 | `unstreamM = unstreamPrimM_IO` — specialise to IO |
| 37 | `"unstreamM[ST]"` | L2621 | `unstreamM = unstreamPrimM_ST` — specialise to ST |

---

## 5. All Rules in vector (Data/Vector/Generic/New.hs)

### 5a. Transform composition

| # | Rule Name | Line | Description |
|---|-----------|------|-------------|
| 1 | `"transform/transform [New]"` | L97 | `transform f1 g1 (transform f2 g2 p) = transform (f1 . f2) (g1 . g2) p` — compose transforms |
| 2 | `"transform/unstream [New]"` | L103 | `transform f g (unstream s) = unstream (Bundle.inplace f g s)` — fold transform into inplace on bundle |

### 5b. Reverse transform composition

| # | Rule Name | Line | Description |
|---|-----------|------|-------------|
| 3 | `"transformR/transformR [New]"` | L123 | `transformR f1 g1 (transformR f2 g2 p) = transformR (f1 . f2) (g1 . g2) p` |
| 4 | `"transformR/unstreamR [New]"` | L130 | `transformR f g (unstreamR s) = unstreamR (Bundle.inplace f g s)` |

### 5c. Slicing through unstream (New-level)

| # | Rule Name | Line | Description |
|---|-----------|------|-------------|
| 5 | `"slice/unstream [New]"` | L171 | `slice i n (unstream s) = unstream (Bundle.slice i n s)` |
| 6 | `"init/unstream [New]"` | L174 | `init (unstream s) = unstream (Bundle.init s)` |
| 7 | `"tail/unstream [New]"` | L177 | `tail (unstream s) = unstream (Bundle.tail s)` |
| 8 | `"take/unstream [New]"` | L180 | `take n (unstream s) = unstream (Bundle.take n s)` |
| 9 | `"drop/unstream [New]"` | L183 | `drop n (unstream s) = unstream (Bundle.drop n s)` |
| 10 | `"unsafeSlice/unstream [New]"` | L186 | `unsafeSlice i n (unstream s) = unstream (Bundle.slice i n s)` |
| 11 | `"unsafeInit/unstream [New]"` | L189 | `unsafeInit (unstream s) = unstream (Bundle.init s)` |
| 12 | `"unsafeTail/unstream [New]"` | L192 | `unsafeTail (unstream s) = unstream (Bundle.tail s)` |

---

## 6. Rules in vector (Storable-specific, not applicable to pvector)

| # | Rule Name | File | Line | Description |
|---|-----------|------|------|-------------|
| 1 | `"unsafeFromForeignPtr fp 0 n -> unsafeFromForeignPtr0 fp n"` | Storable.hs | L2017 | Optimise zero-offset ForeignPtr |
| 2 | `"unsafeFromForeignPtr fp 0 n -> unsafeFromForeignPtr0 fp n"` | Storable/Mutable.hs | L867 | Same for mutable Storable vectors |

---

## 7. All Rules in pvector

### 7a. Stream-level composition rules (Data/PVector/Internal/Stream.hs)

| # | Rule Name | Line | Description |
|---|-----------|------|-------------|
| 1 | `"smap/smap"` | L616 | `smap f (smap g s) = smap (f . g) s` — map composition |
| 2 | `"sfilter/sfilter"` | L619 | `sfilter f (sfilter g s) = sfilter (\x -> g x && f x) s` — filter composition |
| 3 | `"smap/sfilter"` | L622 | `sfilter p (smap f s) = smap f (sfilter (p . f) s)` — push filter before map |
| 4 | `"smap/stake"` | L625 | `stake n (smap f s) = smap f (stake n s)` — push map inside take |
| 5 | `"smap/sdrop"` | L626 | `sdrop n (smap f s) = smap f (sdrop n s)` — push map inside drop |
| 6 | `"stake/sfilter"` | L629 | `stake n (sfilter p s) = sfilter p (stake n s)` — filter through take |
| 7 | `"smap/stakeWhile"` | L632 | `stakeWhile p (smap f s) = smap f (stakeWhile (p . f) s)` |
| 8 | `"smap/sdropWhile"` | L633 | `sdropWhile p (smap f s) = smap f (sdropWhile (p . f) s)` |
| 9 | `"inplace/inplace [pvector]"` | L636 | `inplace f1 g1 (inplace f2 g2 s) = inplace (f1 . f2) (g1 . g2) s` |

### 7b. Vector-level rules (Data/PVector/Back.hs)

| # | Rule Name | Line | Description |
|---|-----------|------|-------------|
| 10 | `"pvector/stream/unstream"` | L2644 | `stream (unstream s) = s` — stream/unstream identity |
| 11 | `"pvector/fusion"` | L2648 | `stream (new (fill s)) = s` — full fusion identity |
| 12 | `"pvector/recycling"` | L2652 | `fill (stream (new p)) = p` — array recycling |
| 13 | `"pvector/transform/fill [New]"` | L2658 | `transform f g (fill s) = fill (S.inplace f g s)` — fold transform into inplace on bundle |
| 14 | `"pvector/transform/transform [New]"` | L2664 | `transform f1 g1 (transform f2 g2 p) = transform (f1 . f2) (g1 . g2) p` — compose transforms |
| 15 | `"pvector/mapNew/mapNew"` | L2673 | `mapNew f (mapNew g p) = mapNew (f . g) p` — compose in-place maps |
| 16 | `"pvector/uninplace/map"` | L2677 | `stream (new (mapNew f p)) = S.smap f (stream (new p))` — uninplace for map |
| 17 | `"pvector/map [stream]"` | L2682 | `map f v = unstream (S.smap f (stream v))` — phase [~1] forwarding |
| 18 | `"pvector/filter [stream]"` | L2684 | `filter f v = unstream (S.sfilter f (stream v))` — phase [~1] forwarding |
| 19 | `"pvector/take [stream]"` | L2686 | `take n v = unstream (S.stake n (stream v))` — phase [~1] forwarding |
| 20 | `"pvector/drop [stream]"` | L2688 | `drop n v = unstream (S.sdrop n (stream v))` — phase [~1] forwarding |
| 21 | `"pvector/takeWhile [stream]"` | L2690 | `takeWhile p v = unstream (S.stakeWhile p (stream v))` — phase [~1] forwarding |
| 22 | `"pvector/dropWhile [stream]"` | L2692 | `dropWhile p v = unstream (S.sdropWhile p (stream v))` — phase [~1] forwarding |
| 23 | `"pvector/zipWith [stream]"` | L2694 | `zipWith f v1 v2 = unstream (S.szipWith f (stream v1) (stream v2))` |
| 24 | `"pvector/mapMaybe [stream]"` | L2696 | `mapMaybe f v = unstream (S.smapMaybe f (stream v))` |
| 25 | `"pvector/foldlDirect/unstream"` | L2702 | `foldlDirect f z (unstream s) = S.sfoldl' f z s` — consumer fusion |
| 26 | `"pvector/foldrDirect/unstream"` | L2704 | `foldrDirect f z (unstream s) = S.sfoldr f z s` — consumer fusion |
| 27 | `"pvector/map [direct]"` | L2709 | `unstream (S.smap f (stream v)) = mapDirect f v` — phase [1] fallback |
| 28 | `"pvector/filter [direct]"` | L2711 | `unstream (S.sfilter f (stream v)) = filterDirect f v` |
| 29 | `"pvector/take [direct]"` | L2713 | `unstream (S.stake n (stream v)) = takeDirect n v` |
| 30 | `"pvector/drop [direct]"` | L2715 | `unstream (S.sdrop n (stream v)) = dropDirect n v` |
| 31 | `"pvector/takeWhile [direct]"` | L2717 | `unstream (S.stakeWhile p (stream v)) = takeWhileDirect p v` |
| 32 | `"pvector/dropWhile [direct]"` | L2719 | `unstream (S.sdropWhile p (stream v)) = dropWhileDirect p v` |
| 33 | `"pvector/zipWith [direct]"` | L2721 | `unstream (S.szipWith f (stream v1) (stream v2)) = zipWithDirect f v1 v2` |
| 34 | `"pvector/mapMaybe [direct]"` | L2723 | `unstream (S.smapMaybe f (stream v)) = mapMaybeDirect f v` |

---

## 8. Comparison Table

### Legend
- **HAS** = pvector has an equivalent rule
- **MISSING** = pvector does NOT have this rule (needs porting)
- **N/A** = not applicable to pvector's design (Storable, Bundle-level, etc.)
- **DIFFERENT** = pvector has a different approach

---

### 8a. Core Stream Fusion Rules (Generic.hs §4f)

| vector Rule | pvector Equivalent | Status |
|---|---|---|
| `"stream/unstream [Vector]"` — `stream (new (New.unstream s)) = s` | `"pvector/fusion"` — `stream (new (fill s)) = s` | **HAS** (pvector uses `fill` instead of `New.unstream`) |
| `"New.unstream/stream [Vector]"` — `New.unstream (stream v) = clone v` | `"pvector/recycling"` — `fill (stream (new p)) = p` | **HAS** (different form but same purpose: avoid materialise-then-stream) |
| `"clone/new [Vector]"` — `clone (new p) = p` | (no `clone` concept) | **N/A** — pvector doesn't have a separate `clone` function |
| `"inplace [Vector]"` — `New.unstream (inplace f g (stream (new m))) = New.transform f g m` | `"pvector/transform/fill [New]"` — `transform f g (fill s) = fill (S.inplace f g s)` | **HAS** (equivalent via different decomposition) |
| `"uninplace [Vector]"` — `stream (new (New.transform f g m)) = inplace f g (stream (new m))` | `"pvector/uninplace/map"` — partial (only for `mapNew`) | **PARTIAL** — pvector only uninplaces `mapNew`, not general `transform` |

### 8b. Stream/Unstream Identity

| vector Rule | pvector Equivalent | Status |
|---|---|---|
| (implicit in vector: `stream . unstream` is handled via the `new (New.unstream s)` path) | `"pvector/stream/unstream"` — `stream (unstream s) = s` | **HAS** (pvector has this as an explicit shortcut) |

### 8c. Right-to-Left (Reverse) Stream Fusion (Generic.hs §4g)

| vector Rule | pvector Equivalent | Status |
|---|---|---|
| `"streamR/unstreamR [Vector]"` | — | **MISSING** |
| `"New.unstreamR/streamR/new [Vector]"` | — | **MISSING** |
| `"New.unstream/streamR/new [Vector]"` | — | **MISSING** |
| `"New.unstreamR/stream/new [Vector]"` | — | **MISSING** |
| `"inplace right [Vector]"` | — | **MISSING** |
| `"uninplace right [Vector]"` | — | **MISSING** |

### 8d. Consumer Forwarding — Indexing Through Unstream (Generic.hs §4a)

| vector Rule | pvector Equivalent | Status |
|---|---|---|
| `"(!)/unstream [Vector]"` | — | **MISSING** |
| `"(!?)/unstream [Vector]"` | — | **MISSING** |
| `"head/unstream [Vector]"` | — | **MISSING** |
| `"last/unstream [Vector]"` | — | **MISSING** |
| `"unsafeIndex/unstream [Vector]"` | — | **MISSING** |
| `"unsafeHead/unstream [Vector]"` | — | **MISSING** |
| `"unsafeLast/unstream [Vector]"` | — | **MISSING** |

### 8e. Monadic Consumer Forwarding (Generic.hs §4b)

| vector Rule | pvector Equivalent | Status |
|---|---|---|
| `"indexM/unstream [Vector]"` | — | **MISSING** (pvector is not monadic-indexed) |
| `"headM/unstream [Vector]"` | — | **MISSING** |
| `"lastM/unstream [Vector]"` | — | **MISSING** |
| `"unsafeIndexM/unstream [Vector]"` | — | **MISSING** |
| `"unsafeHeadM/unstream [Vector]"` | — | **MISSING** |
| `"unsafeLastM/unstream [Vector]"` | — | **MISSING** |

### 8f. Slicing Through `new` (Generic.hs §4c)

| vector Rule | pvector Equivalent | Status |
|---|---|---|
| `"init/new [Vector]"` | — | **MISSING** |
| `"tail/new [Vector]"` | — | **MISSING** |
| `"take/new [Vector]"` | — | **MISSING** |
| `"drop/new [Vector]"` | — | **MISSING** |
| `"unsafeSlice/new [Vector]"` | — | **MISSING** |
| `"unsafeInit/new [Vector]"` | — | **MISSING** |
| `"unsafeTail/new [Vector]"` | — | **MISSING** |

### 8g. Slicing Through `unstream` at New Level (New.hs §5c)

| vector Rule | pvector Equivalent | Status |
|---|---|---|
| `"slice/unstream [New]"` | — | **MISSING** |
| `"init/unstream [New]"` | — | **MISSING** |
| `"tail/unstream [New]"` | — | **MISSING** |
| `"take/unstream [New]"` | — | **MISSING** |
| `"drop/unstream [New]"` | — | **MISSING** |
| `"unsafeSlice/unstream [New]"` | — | **MISSING** |
| `"unsafeInit/unstream [New]"` | — | **MISSING** |
| `"unsafeTail/unstream [New]"` | — | **MISSING** |

### 8h. Miscellaneous Vector Rules

| vector Rule | pvector Equivalent | Status |
|---|---|---|
| `"dropWhile/unstream [Vector]"` | pvector uses `"pvector/dropWhile [stream]"` + fusion | **DIFFERENT** — pvector uses stream-forwarding+fallback approach instead |
| `"unstablePartition"` | — | **MISSING** |
| `"unsafeThaw/new [Vector]"` | — | **N/A** — pvector is immutable persistent |
| `"thaw/new [Vector]"` | — | **N/A** — pvector is immutable persistent |
| `"unstreamM[IO]"` | — | **N/A** — pvector doesn't use monadic unstream |
| `"unstreamM[ST]"` | — | **N/A** — pvector doesn't use monadic unstream |

### 8i. Transform Composition (New.hs §5a-5b)

| vector Rule | pvector Equivalent | Status |
|---|---|---|
| `"transform/transform [New]"` | `"pvector/transform/transform [New]"` | **HAS** |
| `"transform/unstream [New]"` | `"pvector/transform/fill [New]"` | **HAS** (uses `fill` instead of `unstream`) |
| `"transformR/transformR [New]"` | — | **MISSING** (reverse transform) |
| `"transformR/unstreamR [New]"` | — | **MISSING** (reverse transform) |

### 8j. Inplace Composition (Bundle.hs)

| vector Rule | pvector Equivalent | Status |
|---|---|---|
| `"inplace/inplace [Vector]"` | `"inplace/inplace [pvector]"` | **HAS** |

### 8k. Bundle-Level Self-Zip Optimisation (Bundle/Monadic.hs)

| vector Rule | pvector Equivalent | Status |
|---|---|---|
| `"zipWithM xs xs [Vector.Bundle]"` | — | **MISSING** |

### 8l. enumFromTo Specialisations (Stream/Monadic.hs + Bundle/Monadic.hs)

| vector Rule | pvector Equivalent | Status |
|---|---|---|
| All 16+ `enumFromTo<T>` rules | — | **N/A** — pvector doesn't expose an `Enum`-based construction API |

### 8m. reVector Rules

| vector Rule | pvector Equivalent | Status |
|---|---|---|
| `"reVector [Vector]"` | — | **N/A** — pvector doesn't need phantom-type re-tagging |
| `"reVector/reVector [Vector]"` | — | **N/A** |

### 8n. Storable-Specific Rules

| vector Rule | pvector Equivalent | Status |
|---|---|---|
| `"unsafeFromForeignPtr fp 0 n -> unsafeFromForeignPtr0 fp n"` (×2) | — | **N/A** — pvector is not Storable-backed |

---

## 9. Rules that pvector has but vector does NOT

| pvector Rule | Description |
|---|---|
| `"smap/smap"` | Stream-level map composition |
| `"sfilter/sfilter"` | Stream-level filter composition |
| `"smap/sfilter"` | Push filter before map at stream level |
| `"smap/stake"` | Push map inside take at stream level |
| `"smap/sdrop"` | Push map inside drop at stream level |
| `"stake/sfilter"` | Filter through take at stream level |
| `"smap/stakeWhile"` | takeWhile through map at stream level |
| `"smap/sdropWhile"` | dropWhile through map at stream level |
| `"pvector/mapNew/mapNew"` | Compose in-place maps (pvector-specific) |
| `"pvector/uninplace/map"` | Uninplace for mapNew (pvector-specific) |
| `"pvector/map [stream]"` + `[direct]` pair | Phase-controlled forwarding/fallback for map |
| `"pvector/filter [stream]"` + `[direct]` pair | Phase-controlled forwarding/fallback for filter |
| `"pvector/take [stream]"` + `[direct]` pair | Phase-controlled forwarding/fallback for take |
| `"pvector/drop [stream]"` + `[direct]` pair | Phase-controlled forwarding/fallback for drop |
| `"pvector/takeWhile [stream]"` + `[direct]` pair | Phase-controlled forwarding/fallback for takeWhile |
| `"pvector/dropWhile [stream]"` + `[direct]` pair | Phase-controlled forwarding/fallback for dropWhile |
| `"pvector/zipWith [stream]"` + `[direct]` pair | Phase-controlled forwarding/fallback for zipWith |
| `"pvector/mapMaybe [stream]"` + `[direct]` pair | Phase-controlled forwarding/fallback for mapMaybe |
| `"pvector/foldlDirect/unstream"` | Consumer fusion for strict left fold |
| `"pvector/foldrDirect/unstream"` | Consumer fusion for right fold |

**Note:** Vector achieves similar stream-level fusion through `INLINE_FUSED`/`INLINE_INNER` on its `Stream` step functions rather than explicit composition rules. pvector's explicit stream-stream rules (`smap/smap`, etc.) are a design choice — they provide an extra safety net for fusion that vector relies on GHC's simplifier to achieve.

---

## 10. Summary: Rules MISSING from pvector that should be considered for porting

### HIGH PRIORITY (likely beneficial)

| # | vector Rule | Why it matters |
|---|---|---|
| 1 | `"(!)/unstream [Vector]"` | Avoids materialising a vector just to index it |
| 2 | `"(!?)/unstream [Vector]"` | Same for safe indexing |
| 3 | `"head/unstream [Vector]"` | Avoids materialising a vector just to take head |
| 4 | `"last/unstream [Vector]"` | Same for last |
| 5 | `"unsafeIndex/unstream [Vector]"` | Same, unsafe variant |
| 6 | `"unsafeHead/unstream [Vector]"` | Same |
| 7 | `"unsafeLast/unstream [Vector]"` | Same |
| 8 | `"uninplace [Vector]"` (general) | Currently pvector only uninplaces `mapNew`, not general `transform` |
| 9 | `"unstablePartition"` | Avoids materialising intermediate vector in partition |
| 10 | `"zipWithM xs xs [Vector.Bundle]"` | Self-zip optimisation (if pvector supports monadic zip) |

### MEDIUM PRIORITY (requires design decisions about slicing/New layer)

| # | vector Rule | Why it matters |
|---|---|---|
| 11-17 | `"init/new"`, `"tail/new"`, `"take/new"`, `"drop/new"`, `"unsafeSlice/new"`, `"unsafeInit/new"`, `"unsafeTail/new"` | Push slicing through `new` to avoid materialising then slicing |
| 18-25 | `"slice/unstream [New]"`, `"init/unstream [New]"`, etc. (all 8 New-level slice rules) | Fold slicing into the bundle before materialisation |

### LOW PRIORITY (may not apply to pvector's architecture)

| # | vector Rule | Reason for low priority |
|---|---|---|
| 26-31 | All 6 `streamR`/`unstreamR` reverse rules | pvector likely doesn't have right-to-left streaming |
| 32-33 | `"transformR/transformR"`, `"transformR/unstreamR"` | Same — reverse transform infrastructure |
| 34-39 | All 6 monadic indexing rules (`indexM/unstream`, etc.) | pvector doesn't use monadic indexing |
| 40-41 | `"unsafeThaw/new"`, `"thaw/new"` | pvector is persistent/immutable, no thaw |
| 42-43 | `"unstreamM[IO]"`, `"unstreamM[ST]"` | pvector doesn't use monadic unstream |
| 44 | `"clone/new [Vector]"` | pvector doesn't have a `clone` function |
| 45-60 | All `enumFromTo<T>` specialisation rules | pvector doesn't expose Enum-based construction |
| 61-62 | `"reVector"` rules | pvector doesn't need phantom type re-tagging |
| 63-64 | Storable `unsafeFromForeignPtr` rules | Not applicable |

---

## 11. Total Rule Counts

| Package / File | Rule Count |
|---|---|
| **vector-stream** Data/Stream/Monadic.hs | 18 (16 enumFromTo + 2 reVector, all in dead/legacy code) |
| **vector** Data/Vector/Fusion/Bundle/Monadic.hs | 19 (16 enumFromTo + 1 zipWithM self-zip + 2 reVector) |
| **vector** Data/Vector/Fusion/Bundle.hs | 1 (inplace/inplace) |
| **vector** Data/Vector/Generic.hs | 37 |
| **vector** Data/Vector/Generic/New.hs | 12 |
| **vector** Data/Vector/Storable.hs | 1 |
| **vector** Data/Vector/Storable/Mutable.hs | 1 |
| **TOTAL vector + vector-stream** | **89** |
| | |
| **pvector** Data/PVector/Internal/Stream.hs | 9 |
| **pvector** Data/PVector/Back.hs | 26 |
| **TOTAL pvector** | **35** |
