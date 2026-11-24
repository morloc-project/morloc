# Sources

How a term's sources are chosen.

- **A script's local modules imported by name are not its own.** [read,
  2026-11-20] SRC-9 says a script's own modules are the modules of no
  package. shadowImportedSources (Frontend/Treeify.hs ~83-87) counts, for a
  script, only the root module and modules whose name starts with `.`, so
  a module with no package imported by a plain name does not shadow an
  imported source of the same term, and the two stay ambiguous. Reproduce
  with a script importing a package-less sibling module by name; confirm
  how such a module resolves before fixing.
