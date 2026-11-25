# Packet header

- **Header fields are never validated.** [read, 2026-11-15] PLANE-3,
  FMT-1. `is_valid` (morloc-runtime-types/src/packet.rs) checks only the
  magic; the plane, version and flavor fields are always written as 0 and
  never read, so a packet of another plane or format version is accepted.
  The plane field is spelled `plain` (packet.rs, data/morloc/morloc.h).
  So PLANE-4 deviates too: binary data of another plane is accepted, and
  JSON is not the only form that crosses planes.
- **Programs record no plane.** [gap, 2026-11-15] PLANE-5.
