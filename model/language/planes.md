# Planes

A plane is a world of modules with its own standard library and wire
types. Planes never mix. Prefix: PLANE.

### PLANE-1 An environment has exactly one plane
Intent: ruled 2026-11-15
Tests: spec-plane-1-1, spec-plane-1-2, spec-plane-1-3
Code: deviates (issues/packages.md)

The plane is fixed when the environment's state is initialized and
recorded in the state's layout file. No command changes it. Modules are
installed at `<state>/src/morloc/plane/<library>/<name>`. A command run
against a state whose layout is not the compiler's refuses to run and
names the command that rebuilds the state.

### PLANE-2 A state's stores belong to its plane
Intent: ruled 2026-11-15
Code: conforms 2026-11-25

Module records, exposed resources, installed programs, snapshots and the
origins file carry no plane key: each belongs to the one plane of its
state.

### PLANE-3 A binary reader rejects data of another plane
Intent: ruled 2026-11-15
Code: deviates (../runtime/issues/packet-header.md)

A packet carries the id of its plane, its format version and its flavor.
A reader of a packet, a packet file or a stream rejects one whose plane,
version or flavor is not its own.

### PLANE-4 JSON is how data crosses planes
Intent: ruled 2026-11-15
Code: deviates (../runtime/issues/packet-header.md)

JSON carries no plane. It is the only form in which data passes between
planes.

### PLANE-5 A program runs only in its plane
Intent: ruled 2026-11-15
Code: deviates (../runtime/issues/packet-header.md)

A built program records its plane, and the nexus refuses to run it in an
environment of another plane.
