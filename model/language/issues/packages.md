# Packages

- **Installed programs of two libraries may share a name.** [gap,
  2026-11-18] PKG-9. `morloc install --build` names the program after the
  module (executable/Subcommands.hs `buildInstalledModules`); nothing
  refuses a second library's program of the same name.
- **No package server.** [gap, 2026-11-18] PKG-10. The HTTP registry route
  was removed; nothing serves (library, name, hash) tarballs yet.
- **Freezes do not yet write library lines.** [gap, 2026-11-18] PKG-7.
  meco's freeze writes `name hash` and does not refuse a locally installed
  module (tooling/meco lib/mcfrz.rs).
- **The layout file records no plane.** [read, 2026-11-18] PLANE-1. The
  plane's `LAYOUT` file holds only the layout version; the plane's id,
  which PLANE-3 and PLANE-5 would check, is not recorded anywhere.
