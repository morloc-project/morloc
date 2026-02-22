# Test installation

These suite tests the `morloc make --install` command.

Each test verifies 6 things:

1. Binary installed to ~/.local/share/morloc/bin/
2. Exe directory created under ~/.local/share/morloc/exe/
3. Pools directory / included files/folders copied correctly
4. Program produces correct output when run
5. morloc uninstall --program removes the binary
6. Cleanup is complete

  ┌──────────┬──────────┬───────────────────────────────────────────────────────────────────┐
  │  Module  │ Language │                           What it tests                           │
  ├──────────┼──────────┼───────────────────────────────────────────────────────────────────┤
  │ testpy1  │ Python   │ Direct source in cwd, file include via package.yaml               │
  ├──────────┼──────────┼───────────────────────────────────────────────────────────────────┤
  │ testpy2  │ Python   │ Source in src/, whole folder include via package.yaml             │
  ├──────────┼──────────┼───────────────────────────────────────────────────────────────────┤
  │ testpy3  │ Python   │ Direct source + indirect Python import, include via --include CLI │
  ├──────────┼──────────┼───────────────────────────────────────────────────────────────────┤
  │ testcpp1 │ C++      │ Direct .hpp source in cwd, file include via package.yaml          │
  ├──────────┼──────────┼───────────────────────────────────────────────────────────────────┤
  │ testcpp2 │ C++      │ Source in src/, whole folder include via package.yaml             │
  ├──────────┼──────────┼───────────────────────────────────────────────────────────────────┤
  │ testcpp3 │ C++      │ Direct source + indirect #include, include via --include CLI      │
  ├──────────┼──────────┼───────────────────────────────────────────────────────────────────┤
  │ testr1   │ R        │ Direct source in cwd, file include via package.yaml               │
  ├──────────┼──────────┼───────────────────────────────────────────────────────────────────┤
  │ testr2   │ R        │ Source in src/, whole folder include via package.yaml             │
  ├──────────┼──────────┼───────────────────────────────────────────────────────────────────┤
  │ testr3   │ R        │ Direct source + indirect source(), include via --include CLI      │
  └──────────┴──────────┴───────────────────────────────────────────────────────────────────┘

