# Word puzzle solver architecture and design

## 1. Project overview

The word puzzle solver is a command-line application implemented in Haskell. It
is designed to solve various word puzzles such as "Nine Letter Word", "Your Wise
Life", "NYT Spelling Bee", and "Scientific American Spellements". The core of
the application is a highly efficient stream processing engine that filters a
dictionary against puzzle-specific constraints.

## 2. Goals and non-goals

### 2.1 Goals

* Provide a fast, memory-efficient solver using stream processing.
* Validate all command-line inputs strictly and report all errors together.
* Support both standard puzzles (nine-letter constraint, single use) and pelling
  Bee variants (unlimited length, reusable letters).
* Maintain a robust continuous integration pipeline (GitHub Actions and itLab
  CI).

### 2.2 Non-goals (Out of scope)

* A graphical user interface (GUI) or web front-end.
* Support for puzzles requiring spatial logic (e.g., crosswords).
* Dynamic fetching of dictionaries from the internet at runtime.

## 3. Requirements

### 3.1 Functional requirements

* **Input parameters**:
  * Accept a minimum word size constraint (between four and nine inclusive).
  * Accept a mandatory letter constraint (single lowercase letter).
  * Accept a string of allowed letters (four to nine unique lowercase letters).
  * Provide an optional dictionary file path (defaulting to "dictionary").
  * Support a "repeats" toggle to allow letters to be reused.
* **Validation**: Collect and report all input validation errors rather than
  failing on the first encountered error.
* **Processing rules**:
  * Word length must be greater than or equal to the minimum size.
  * Word must contain the mandatory character.
  * Word must contain only characters from the allowed letter pool.
  * In standard mode, letters cannot be repeated beyond their occurrence in the
    pool (maximum length nine).
  * In repeat mode, letters can be reused infinitely, and words can exceed nine
    characters, up to a defensive upper bound of 50 characters.
* **Output**: Print matching words to standard output.

### 3.2 Non-functional requirements

* **Performance**: Minimise memory residency and garbage collection overhead by
  streaming dictionary data from disk to standard output without loading the
  entire dictionary into memory.
* **Robustness**: Validate all inputs strictly using applicative parsing.
* **Documentation**: Provide generated Haddock documentation and maintain a
  project glossary.

## 4. Architecture design

### 4.1 Core components

The architecture separates command-line parsing from core domain logic.

* **Command-line parser (`app/Main.hs`)**: Utilises `optparse-applicative` to
  parse inputs and `Data.Validation` to accumulate validation errors. Constructs
  the `WordPuzzle` configuration.

* **Domain logic and solver (`src/WordPuzzle.hs`)**: Houses the `WordPuzzle`
  data structures and the `solver` engine. The solver is decoupled from direct
  input/output by operating on `InputStream ByteString` types.

### 4.2 Data models

* `WordPuzzle`: A record containing the parameters of a specific run: `size`,
  `mandatory`, `letters`, `dictionary`, and `repeats`. The data constructor is
  hidden from external modules; all construction must go through the
  `mkWordPuzzle` smart constructor, which validates inputs before returning a
  value.
* `ValidationError`: A sum type encapsulating parsing errors: `InvalidSize`,
  `InvalidLetters`, `UnexpectedValue`, `InvalidMandatory`, and
  `MandatoryNotInLetters`.

### 4.3 Execution flow

1. **Initialisation**: The user invokes the executable with CLI arguments.
2. **Validation**: `Main.hs` parses the arguments and passes them to
   `mkWordPuzzle`, the smart constructor. If validation fails, all errors are
   printed to stderr and the program halts. If successful, a validated
   `WordPuzzle` value is returned.
3. **Stream processing**: `WordPuzzle.solve` opens the dictionary file using
   `System.IO.Streams.withFileAsInput`.
4. **Transformation**: The raw byte stream is converted to a stream of lines.
5. **Filtering**: The `solver` function constructs a composite `Predicate`
   (length check, mandatory check, and letter pool check) and filters the
   stream. The letter pool check leverages low-level bitwise operations for high
   performance during the standard puzzle mode.
6. **Output**: Surviving words are mapped directly to standard output.

## 5. Design alternatives and trade-offs

* **In-memory dictionary vs. streaming**: An alternative approach would be to
  load the entire dictionary into a tree or trie data structure in memory.
  However, in Haskell, streaming is often much faster for this type of
  workload. Rather than just saving memory, a single-pass stream avoids the
  overhead of building large intermediate data structures on the heap,
  bypassing significant garbage collection pauses and initialisation time
  that a trie would incur.

* **Unboxed bitwise types vs. wrapped strings**: For the letter pool check,
  we leverage unwrapped/unboxed bitwise types instead of relying on wrapped
  string types (`String` or `Text`). In Haskell, primitive unboxed types
  eliminate pointer chasing and heap allocations, providing a massive
  performance improvement in tight filtering loops.

* **Applicative vs. monadic validation**: Monadic parsing would halt on the
  first error. We opted for the applicative `Data.Validation` approach to
  collect and display all configuration errors at once, leading to a vastly
  superior user experience. While applicative validation makes dependent
  validation (where one rule depends on the success of another) more
  complex, dependent validation is explicitly not envisioned for this tool.
  This allows us to maintain simple and exhaustive error reporting.

* **Smart constructor pattern vs. open constructor**: Exporting the raw
  `WordPuzzle` data constructor would allow consumers to bypass validation and
  create values with invalid invariants (e.g. size 0, uppercase letters). By
  hiding the constructor and exporting only `mkWordPuzzle`, we ensure every
  `WordPuzzle` value is validated at construction time. This mirrors Lean 4's
  `private mk` pattern using standard Haskell (no extensions). The trade-off
  is that test code cannot directly construct values and must use the smart
  constructor or a helper that extracts the `Success` case.

## 6. Evaluation and future considerations

The current architecture is extremely efficient. Future improvements could focus
on:

* **Rule extensibility**: While currently the `solver` interface is tightly
  coupled to existing rules, a strictly long-term goal could involve introducing
  a plugin architecture or domain-specific language for defining custom puzzle
  predicates.

