# Language registry

No single object describes a language. lang.yaml is parsed three times
(LangRegistry's entry, LanguageDescriptor, the `declared*` helpers), and
API, Treeify, Generic and Express each do their own embedded-then-plugin
lookup with their own defaults and error handling. Facts the description
should own live in Haskell and templates instead (Common.manNamer,
Language.makeExecutableName's compiled list, Pool.memberFor). [read,
2026-10-23]

- **Plugin languages half work.** Needs a ruling: are languages under
  `<home>/lang/*` supported? If not, delete the plugin path (scanLangDir and
  the filesystem fallbacks in Treeify and Generic); if so, one loader in
  runMorlocMonad must build the registry from embedded and plugin files,
  fail on a malformed file, and every consumer must read only it.
  Today `extendRegistry` (LangRegistry.hs:99) has no caller, so a plugin's
  `preamble`, `run_command`, `is_compiled`, `allow_string_null`, `host`
  and `aliases` fall back to defaults (a plugin pool runs as
  `<langname> <pool>`; `allow_string_null: false` gets no NUL check at the
  literal stage or the boundary); a malformed plugin lang.yaml is skipped
  silently (API.hs `scanLangDir`), so the user sees "unknown language";
  plugins hit "Unsupported language" in Serial.hs on passthrough types;
  descriptors are found by directory name in Treeify and Generic but by the
  yaml `name:` in discovery, so the two can disagree. No spec item, docs or
  test covers plugins.
- **Dead registry functions.** Unused: registryMakeExecutablePoolName, registryMakeSourcePoolName,
  registryMakeExtension, registrySerialType, Monad.readLang.
