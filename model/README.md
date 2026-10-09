# model/

The specification of morloc, as three specs held to one standard:

- `language/`: what morloc programs mean. Which programs the compiler
  accepts, what type each term has, what each term computes, and what a
  value looks like in each language it reaches.
- `compiler/`: the compiler's internal contracts. What each pass may assume
  of its input and must guarantee of its output. Derived from `language/`.
- `runtime/`: processes, threads, locks, shared memory, fork, the daemon,
  streams and panics in the built program. Its PlusCal models are in
  `runtime/tla/`.

The spec states intent. The code is measured against it, not the reverse.

## Items

Every rule is one item with a stable ID:

    ### MOD-16 Importing a type and declaring the same name is a load error
    Intent: ruled 2026-10-04
    Tests: spec-mod-16-1, spec-mod-16-2
    Code: conforms 2026-10-07

    The rule, as briefly as possible, in notation where notation is clearer
    than words.

    Why: one line, only where the ruling was contested or reversed.

- `Intent` is one of:
  - `ruled <date>`: the user decided it. Only the user moves an item here.
  - `proposed`: a draft from the docs, the code or an earlier design. A
    best guess, never authority.
  - `open`: undecided. The body states the question and the candidate
    answers.
  - `retired <date>`: no longer a rule. The item stays so that its ID is
    never reused.
- `Code` is `unaudited`, `conforms <date>`, or `deviates <ref>`, where
  `<ref>` names an issue (`#160`) or a finding.
- `Tests` lists the tests that are the item's examples. A `ruled` item
  cites at least one; a `proposed` item may; an `open` or `retired` item
  cites none. An item with no tests omits the line. `check.sh` holds the
  chapters listed as complete in `language/README.md` to this, and to a
  ruled item conforming; elsewhere it warns.
- IDs are `<PREFIX>-<n>`, never reused or renumbered. Each prefix belongs
  to one file, and prefixes are unique across all three specs.
- A rule on a boundary between specs lives in one of them, and the other
  cites it by ID. Exit status 70 after a panic is `runtime/panic.md`'s, and
  a language item that needs it says "PANIC".
- The spec has no inline examples. Every example is a test.
- ASCII only.

## Prefixes

| Spec | Prefixes |
|---|---|
| runtime | FORK SHM DAEMON SLOT STATE INIT PANIC NET |
| language | see `language/README.md` |
| compiler | see `compiler/README.md` |

## Tests

- A test is named `spec-<prefix>-<rule>-<n>`, prefix in lower case:
  `spec-mod-14-1` is the first example of MOD-14.
- A unit test is the default: a case in `test-suite/SpecTests.hs` whose name
  is its ID. It covers anything decided before code generation.
- A golden test is `test-suite/golden-tests/spec-*/`, used only when the
  rule needs a build, a run, or more than one language.
- A rejection test has an accept twin that differs only in the rule's
  subject, since a rejection test passes on any error.
- No test checks the wording of an error message.
- A test for a `deviates` item fails until the code is fixed, and its
  header names the issue.
- Runtime items also cite Rust test names and `tla:<config>` runs.

## Using the spec

- Before fixing a bug, find the items that govern the behavior.
  - The code breaks a `ruled` item: the code is wrong.
  - The item is `proposed` or `open`, or there is none: the intent is
    unknown. Write or update the item, mark it `open`, and get a ruling
    before deciding the fix.
  - A `ruled` item leads to an unsound or contradictory result: the spec is
    wrong. Mark it `open`, state the contradiction, and get a new ruling.
- A ruling changes the spec in the same change as any code that follows it.
- Audit by picking an item, reading the code that implements it, and
  recording the result in its `Code` field.

## Citing an item from code

A comment is a reference to an item, optionally followed by how the line
applies it:

    // FORK-1: the cache's references are the parent's; forget them.

## The check

`model/check.sh` checks the specs against each other and against their
tests. It fails when:

- an ID is defined twice, or a prefix is used in two files;
- an `Intent` or `Code` field is missing or malformed;
- a `ruled` item cites no test, or an `open` or `retired` item cites one;
- a cited test does not exist, is named for a different item, or is cited
  twice;
- a `spec-` test is cited by no item;
- a spec file contains a non-ASCII character.

It applies the full check to `language/` and `compiler/`. Until the runtime
items migrate to this format, it reads `runtime/` only for IDs and prefixes.
