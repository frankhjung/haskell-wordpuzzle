# Bitmask and PoolMask Optimisation

This document details the design, implementation, and performance impact of the
integer bitmask optimisation used to filter dictionary words in the Haskell
[`wordpuzzle`](../src/WordPuzzle.hs) solver.

## 1. Overview and Motivation

When solving word puzzles, the application filters an entire dictionary
(typically over 63,000 words in `wamerican`) against puzzle constraints. A core
filtering step tests whether each character in a candidate dictionary word
belongs to the puzzle's allowed letter pool.

Historically, this check relied on list traversals:

- In Spelling Bee mode, checking `c 'elem' letters` traversed the letter pool
  for every character in every word.
- In 9-letter mode, `c 'notElem' letters` similarly traversed the pool, while
  letters already seen were tracked in an accumulator bitmask.

While simple, using list membership across 63,000+ words incurs repeated list
scanning and pointer dereferencing. By encoding the 26 lowercase English letters
(`'a'` to `'z'`) as bits within a single machine integer
([`poolMask`](../src/WordPuzzle.hs#L52)), character membership checks are
reduced to single, constant-time bitwise CPU instructions.

## 2. Bitmask Architecture

### 2.1 Mapping Characters to Bits

Each letter from `'a'` to `'z'` corresponds to an integer bit index from `0` to
`25`. The function [`bitOf`](../src/WordPuzzle.hs#L209-L215) computes this
shift:

```haskell
bitOf :: Char -> Int
bitOf c
  | i >= 0 && i < 26 = 1 `unsafeShiftL` i
  | otherwise        = 0
  where
    i = ord c - ord 'a'
```

- `'a'` has ASCII code 97 (`97 - 97 = 0`), producing bit 0 (`1`).
- `'b'` has ASCII code 98 (`98 - 97 = 1`), producing bit 1 (`2`).
- `'z'` has ASCII code 122 (`122 - 97 = 25`), producing bit 25 (`33554432`).
- Any non-lowercase or out-of-range character yields `0`, ensuring safe and
  total evaluation without throwing runtime exceptions.

### 2.2 Constructing the Pool Mask

The pool mask is the bitwise OR union of all character bits in the allowed
pool. The function [`maskOf`](../src/WordPuzzle.hs#L224-L225) folds over the
letters string:

```haskell
maskOf :: String -> Int
maskOf = foldl' (\m c -> m .|. bitOf c) 0
```

For example, the pool `"cadevrsoi"` produces a bitmask with 9 distinct bits
set, representing the exact allowed alphabet.

### 2.3 Elimination of Redundant Work

In the [`WordPuzzle`](../src/WordPuzzle.hs#L48-L56) record, the computed mask is
stored as an unpacked, strict integer field:

```haskell
data WordPuzzle = WordPuzzle
  { size       :: Int
  , mandatory  :: Char
  , letters    :: String
  , dictionary :: FilePath
  , repeats    :: Bool
  , poolMask   :: !Int
  } deriving (Eq, Show)
```

The smart constructor [`mkWordPuzzle`](../src/WordPuzzle.hs#L81-L91) evaluates
`maskOf ls` exactly once during initialisation:

```haskell
mkWordPuzzle r s m ls d =
  WordPuzzle <$> validateSize s
             <*> validateMandatory m
             <*> validateLetters ls
             <*> pure d
             <*> pure r
             <*> pure (maskOf ls)
             <* validateMandatoryInLetters m ls
```

This guarantees that the letter pool is never traversed or folded repeatedly
during stream filtering. The single precomputed `poolMask` is passed down to the
per-word filter predicates.

## 3. Membership Testing

### 3.1 Spelling Bee Mode

In Spelling Bee mode, letters may be repeated indefinitely. The predicate
[`spellingBee`](../src/WordPuzzle.hs#L280-L295) computes the pool mask once upon
partial application and verifies that every character in the ByteString
candidate belongs to the pool:

```haskell
spellingBee :: String -> BS.ByteString -> Bool
spellingBee ls =
  let !pool = maskOf ls
  in BS.all (\c -> bitOf c .&. pool /= 0)
```

For each character:

1. `bitOf c` isolates the bit corresponding to character `c`.
2. Bitwise AND (`.&.`) with `pool` evaluates whether that bit is set.
3. If the result is non-zero, the character belongs to the allowed pool.

### 3.2 Standard 9-Letter Mode

In 9-letter mode, letters must not be repeated. The predicate
[`nineLetters`](../src/WordPuzzle.hs#L250-L278) maintains an accumulator
pair `(seen, ok)` across a strict left fold:

```haskell
nineLetters :: String -> BS.ByteString -> Bool
nineLetters ls =
  let !pool = maskOf ls
  in snd . BS.foldl' (step pool) (0 :: Int, True)
  where
    step pool (seen, ok) c
      | not ok          = (seen, False)
      | b .&. pool == 0 = (seen, False)
      | seen .&. b /= 0 = (seen, False)
      | otherwise       = (seen .|. b, True)
      where
        b = bitOf c
```

Here:

- `pool` is the invariant allowed mask evaluated once via `maskOf ls`.
- `seen` is an accumulator bitmask tracking characters seen so far in the word.
- `b .&. pool == 0` rejects characters outside the pool in $O(1)$ time.
- `seen .&. b /= 0` rejects repeated characters in $O(1)$ time.
- `seen .|. b` records the character for subsequent checks.

## 4. Performance Benchmarks

Benchmarks were measured using Criterion against the full 63,031-word
dictionary (`wamerican`), comparing the unoptimised list traversal against the
precomputed bitmask:

| Benchmark | List Membership (`elem`) | Bitmask (`poolMask`) | Speed-up |
| :--- | :--- | :--- | :--- |
| `nineLetters` | 5.68 ms | 1.26 ms | ~4.5× |
| `spellingBee` | 5.17 ms | 0.34 ms | ~15× |

### Why Bitmasks are Faster

1. **Zero Heap Allocation:** Integer bitwise shifts and logical operations
   execute entirely in CPU registers without allocating heap objects, list
   nodes, or closure environments.
2. **Cache and Branch Efficiency:** Bit operations avoid memory latency and
   pointer chasing inherent in linked list traversals.
3. **One-Time Precomputation:** Computing `poolMask` during smart construction
   means dictionary filtering incurs zero setup overhead inside the streaming
   loop.
