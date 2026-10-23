# Build artifacts

What `morloc make` produces and how the nexus finds it.

### ART-1 A build lives in `<key>-build/` beside the program's sources
Status: draft

The key is `--name`, else `-o`, else the source basename (for an install, the
module name). The directory holds a `.morloc-build` marker, `manifest.json`,
`envspec.json` and `pools/<lang>/`. Interpreted pools load user sources from
the directory that contains the build directory, so the two move together.
No absolute build path is recorded.

### ART-2 A rebuild never removes a directory that is not a Morloc build
Status: draft

An existing `<key>-build` is replaced only if it is a real directory (not a
symlink) carrying the marker.

### ART-3 A failed build leaves the previous build in place
Status: draft

### ART-4 A launcher declares its manifest on a `# morloc-manifest: ` line
Status: draft

A relative path resolves against the launcher's directory. Every nexus mode
that names a program accepts a launcher or a manifest path.

### ART-5 The nexus accepts only a manifest from its own Morloc version
Status: draft

`build.morloc_version` must equal the nexus version exactly; otherwise the
nexus refuses with a message to rebuild. Unknown keys are ignored.
