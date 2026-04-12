//! Morloc manifest schema (v2) -- canonical Rust types.
//!
//! The morloc compiler emits a `.manifest` JSON blob describing every
//! exported command's interface. This crate is the **single source of
//! truth** for that schema's Rust representation. Both the CLI nexus
//! (`morloc-nexus`) and the C-FFI runtime (`morloc-runtime`) depend on
//! these types so neither has to maintain its own deserialization
//! logic.
//!
//! ## Versioning
//!
//! The manifest does not carry a dedicated schema version. Manifests
//! are transient build artifacts (always regenerated on `morloc make`,
//! never stored in version control), so the morloc compiler version
//! recorded in the [`Build`] sub-object serves as the staleness
//! indicator. The check happens in [`parse_manifest`].
//!
//! Version coupling: this crate's `CARGO_PKG_VERSION` is intentionally
//! kept in lockstep with the morloc Haskell compiler version (see
//! `package.yaml`). The same is true of `morloc-nexus` and
//! `morloc-runtime` -- bumping the morloc compiler requires bumping
//! all three Rust crates in the same commit.
//!
//! ## Extension slots
//!
//! Every entity object (manifest, pool, command, arg, return, group,
//! service) carries:
//!
//! - `constraints: Vec<Constraint>` -- enforceable rules. Currently
//!   the compiler emits only the `kind` constraint on named-type args
//!   (record/object/table). Future constraints (`min`, `max`, `regex`,
//!   `length`, `non_empty`, `row_count`, ...) will append to this
//!   list without any schema change.
//!
//! - `metadata: BTreeMap<String, serde_json::Value>` -- free-form
//!   informational key-value pairs. Always emitted as `{}` today;
//!   reserved so consumers never have to check whether the field
//!   exists. Future doc hints, studio annotations, telemetry tags,
//!   etc. live here until they stabilize into first-class fields.
//!
//! Many of these slots are `#[allow(dead_code)]` because no current
//! consumer reads them. They are deliberate forward-compatible
//! placeholders, not vestigial fields.
//!
//! ## Unknown-field tolerance
//!
//! All structs use `#[serde(default)]` on optional fields and silently
//! ignore unknown JSON keys. A manifest written by a newer morloc
//! compiler will still parse with an older nexus (modulo the version
//! mismatch error in [`parse_manifest`]).

use serde::Deserialize;
use std::collections::BTreeMap;

/// Convenient alias for the `metadata` extension slot. Using
/// `BTreeMap` (rather than `HashMap` or raw `serde_json::Value`) gives
/// us (a) compile-time enforcement that metadata is always a JSON
/// object, and (b) deterministic iteration order for stable diffs.
pub type Metadata = BTreeMap<String, serde_json::Value>;

// -- Top-level manifest -------------------------------------------------------

/// The top-level manifest object. Embedded in every built nexus binary
/// as a JSON blob after the `### MANIFEST ###` marker.
#[derive(Debug, Deserialize)]
#[allow(dead_code)]
pub struct Manifest {
    /// Program identifier -- comes from the morloc `module` declaration.
    pub name: String,
    /// Compiler-sourced build metadata (path, timestamp, version).
    /// Distinct from the user-sourced top-level `metadata` slot.
    pub build: Build,
    /// Language pool daemons that this program dispatches to.
    #[serde(default)]
    pub pools: Vec<Pool>,
    /// Exported commands the user can invoke.
    #[serde(default)]
    pub commands: Vec<Command>,
    /// Command groups for organizing CLI subcommands in help output.
    #[serde(default)]
    pub groups: Vec<CmdGroup>,
    /// Daemon-mode service configuration. None for normal CLI mode.
    #[serde(default)]
    pub service: Option<Service>,
    /// **Reserved.** User-sourced free-form annotations on the module.
    /// Always emitted as `{}` today. Distinct from `build` (which is
    /// compiler-sourced).
    #[serde(default)]
    pub metadata: Metadata,
}

/// Compiler-sourced metadata about how this manifest was produced.
///
/// Future build fields (`hash`, `source_hash`, `host`, `user`, `system`,
/// `dependencies`, `cflags`, `reproducible`, ...) will be added directly
/// to this struct as additive non-breaking changes -- no sub-metadata
/// nesting required.
#[derive(Debug, Deserialize)]
#[allow(dead_code)]
pub struct Build {
    /// Absolute path to the build directory containing this program's
    /// pool executables and generated source files. The nexus chdirs
    /// here at startup so relative pool exec paths resolve.
    pub path: String,
    /// Unix timestamp at which the manifest was generated.
    pub time: i64,
    /// Version of the morloc compiler that produced this manifest. The
    /// nexus compares this against its own compile-time
    /// `CARGO_PKG_VERSION` (which is intentionally synchronized with
    /// the morloc compiler version) in [`parse_manifest`]; a mismatch
    /// produces an actionable "rebuild with the current compiler"
    /// error.
    pub morloc_version: String,
}

/// A single language pool daemon. Each pool is one OS process that
/// hosts the language-specific implementations of source functions.
#[derive(Debug, Deserialize)]
#[allow(dead_code)]
pub struct Pool {
    /// Language tag (e.g. `"py"`, `"cpp"`, `"r"`, `"julia"`).
    pub lang: String,
    /// argv used to spawn the pool process (e.g. `["python3", "pool.py"]`).
    pub exec: Vec<String>,
    /// Unix domain socket basename (under tmpdir) for IPC.
    pub socket: String,
    /// **Reserved.** Per-pool metadata. Future slots: `resource`
    /// (cpu/memory limits), `env` (environment variables),
    /// `startup_timeout`, `health_check`.
    #[serde(default)]
    pub metadata: Metadata,
}

// -- Commands -----------------------------------------------------------------

/// Discriminator for the command kind. Closed enum so adding a new
/// variant requires explicit code changes everywhere it's matched.
#[derive(Debug, Deserialize, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum CmdType {
    /// Dispatched to a language pool process via IPC.
    Remote,
    /// Evaluated inline by the nexus from an embedded expression tree.
    Pure,
}

/// One exported morloc function the user can invoke as a CLI subcommand.
#[derive(Debug, Deserialize)]
#[allow(dead_code)]
pub struct Command {
    /// CLI subcommand name (defaults to the morloc function name; can
    /// be overridden via a `--' name:` docstring directive).
    pub name: String,
    /// Discriminator: [`CmdType::Remote`] (dispatch to a pool) or
    /// [`CmdType::Pure`] (evaluate inline via the manifest's `expr`
    /// tree).
    #[serde(rename = "type")]
    pub cmd_type: CmdType,

    // -- Remote-only dispatch info ----------------------------------------
    /// Manifold ID -- the integer key under which the pool's dispatch
    /// table contains this function's entry. Remote commands only.
    #[serde(default)]
    pub mid: u32,
    /// Index into [`Manifest::pools`] for the primary pool that hosts
    /// this command's top-level function. Remote commands only.
    #[serde(default, rename = "pool")]
    pub pool_index: usize,
    /// Indices of every pool transitively required to execute this
    /// command (the primary pool plus any pools called as foreign
    /// functions from inside it). Remote commands only.
    #[serde(default)]
    pub needed_pools: Vec<usize>,

    // -- Common fields ----------------------------------------------------
    /// Description lines shown in CLI help. The first line is the
    /// summary used in subcommand listings.
    #[serde(default)]
    pub desc: Vec<String>,
    /// Argument list, in declaration order. Each entry is a
    /// discriminated union -- see [`Arg`].
    #[serde(default)]
    pub args: Vec<Arg>,
    /// Return-value descriptor. Always present, even for nullary
    /// returns (use a Nil schema in that case).
    #[serde(default, rename = "return")]
    pub ret: Return,
    /// **Reserved.** Command-level constraints -- invariants that span
    /// multiple arguments (e.g. `equal_length` of two list args).
    /// Empty in v2; populated when the constraint system rolls out.
    #[serde(default)]
    pub constraints: Vec<Constraint>,
    /// **Reserved.** Per-command metadata. Future slots: `effects`
    /// (declared I/O / network / filesystem effect set), `resource`
    /// (CPU/memory/time limits), `auth` (required capabilities),
    /// `version` (semantic version of the command's API),
    /// `deprecated` (migration notice).
    #[serde(default)]
    pub metadata: Metadata,

    // -- Pure-only evaluation info ----------------------------------------
    /// Embedded expression tree (NexusExpr JSON) for pure commands.
    /// Evaluated inline by the nexus instead of being dispatched to a
    /// pool. Absent on remote commands.
    #[serde(default)]
    pub expr: Option<serde_json::Value>,

    // -- Command group ----------------------------------------------------
    /// Optional name of the command group this command belongs to. Used
    /// to organize subcommands in help output. The Haskell emitter now
    /// writes a real JSON null for absent groups (see
    /// `Morloc.CodeGenerator.Nexus.cmdGroupField`), so no custom
    /// deserializer is needed.
    #[serde(default)]
    pub group: Option<String>,
}

impl Command {
    pub fn is_pure(&self) -> bool {
        self.cmd_type == CmdType::Pure
    }
}

/// Return-value descriptor. Structurally similar to a typed [`Arg`]
/// minus the CLI-specific fields (kind, metavar, quoted, short/long,
/// default). Always present on every command.
#[derive(Debug, Deserialize, Default)]
#[allow(dead_code)]
pub struct Return {
    /// Morloc serialization schema string for the return type. The
    /// nexus uses this to deserialize the bytes coming back from the
    /// pool process.
    #[serde(default)]
    pub schema: String,
    /// User-facing type name as written in the morloc source (e.g.
    /// `"Int"`, `"Config"`, `"[Int]"`). Used in help output and error
    /// messages. JSON key is `type`; the Rust field is `type_desc`
    /// because `type` is a reserved keyword.
    #[serde(default, rename = "type")]
    pub type_desc: String,
    /// Description lines for the return value, parsed from `--' return:`
    /// docstrings.
    #[serde(default)]
    pub desc: Vec<String>,
    /// **Reserved.** Constraints on the return value. Currently used
    /// only for `kind: record|object|table` on named return types;
    /// future constraints (min/max/regex/...) will live here.
    #[serde(default)]
    pub constraints: Vec<Constraint>,
    /// **Reserved.** Per-return metadata; same forward-compatibility
    /// rationale as the per-arg slot.
    #[serde(default)]
    pub metadata: Metadata,
}

// -- Constraints --------------------------------------------------------------

/// A single constraint entry attached to an arg, return value, or
/// command. Discriminated by `type`.
///
/// **Currently emitted constraint types**:
///
/// - `kind`: marks a named type as `record` / `object` / `table`. The
///   `value` payload is the lowercased name. The CLI help renderer
///   uses this to partition into Record Schemas / Table Schemas
///   sections.
///
/// **Reserved constraint types** (defined shapes, not yet emitted by
/// any compiler pass -- names and payloads chosen so the schema
/// doesn't need to bump when the constraint feature lands):
///
/// - `min`, `max`: numeric bounds with `value: <number>`.
/// - `length`: `value: <int>` for a fixed length, or `{min, max}` for
///   a bounded range.
/// - `non_empty`: list/string must be non-empty (no payload).
/// - `regex`: string must match `value: "<pcre>"`.
/// - `enum`: value must be one of `value: [...]`.
/// - `unique`: list elements must be pairwise distinct (no payload).
/// - `row_count`: fixed/bounded row count for `table`-tagged args.
/// - `sorted`: `value: "asc"|"desc"`.
///
/// **Extensibility rules**:
///
/// 1. Unknown `type` values MUST be silently ignored by readers.
/// 2. Adding a new constraint type does not bump the manifest version.
/// 3. Changing the payload shape of an existing type DOES bump.
/// 4. Constraints are unordered.
/// 5. Multiple constraints of the same type on the same entity are
///    allowed.
#[derive(Debug, Deserialize, Clone)]
#[allow(dead_code)]
pub struct Constraint {
    /// Constraint discriminator. JSON key is `type`; the Rust field is
    /// `ctype` because `type` is a reserved keyword.
    #[serde(rename = "type")]
    pub ctype: String,
    /// Constraint-specific payload. Shape depends on `ctype`. Some
    /// constraint types (`non_empty`, `unique`) carry no payload.
    #[serde(default)]
    pub value: Option<serde_json::Value>,
}

// -- Arguments ----------------------------------------------------------------

/// CLI argument variants. Each command's argument list is a sequence
/// of these, in declaration order.
///
/// Three of the four variants (`Positional`, `Optional`, `Group`)
/// carry type/schema/constraints information because they represent
/// typed values that flow through to a pool. `Flag` is a pure boolean
/// toggle with no associated type -- and therefore no `schema`,
/// `type_desc`, or `constraints` slot.
///
/// `#[allow(dead_code)]` covers the `metadata` slots that are reserved
/// for future use and not read by any current consumer.
#[derive(Debug, Deserialize)]
#[serde(tag = "kind")]
#[allow(dead_code)]
pub enum Arg {
    /// A positional CLI argument.
    #[serde(rename = "pos")]
    Positional {
        /// Morloc serialization schema string. Used at dispatch time
        /// to parse the user's CLI input into a binary data packet.
        #[serde(default)]
        schema: Option<String>,
        /// User-facing type name (e.g. `"Int"`, `"Config"`). The Rust
        /// field is `type_desc` because `type` is a reserved keyword.
        #[serde(default, rename = "type")]
        type_desc: Option<String>,
        /// Display placeholder shown in help (e.g. `"FILE"`). None
        /// falls back to a generic `ARG` placeholder.
        #[serde(default)]
        metavar: Option<String>,
        /// If true, the user's CLI value is JSON-wrapped before being
        /// passed to the pool. Used for `Str`-typed positionals
        /// declared with `--' literal: true`.
        #[serde(default)]
        quoted: bool,
        /// Description lines from `--' desc:` docstrings.
        #[serde(default)]
        desc: Vec<String>,
        /// Per-argument enforceable invariants. Currently only the
        /// `kind` constraint is emitted (for named-type args). Future
        /// constraints (min/max/regex/length/...) will populate this.
        #[serde(default)]
        constraints: Vec<Constraint>,
        /// **Reserved.** Per-argument informational metadata. Always
        /// emitted as `{}` in v2; not yet read by any consumer.
        #[serde(default)]
        metadata: Metadata,
    },
    /// An optional CLI argument with a long/short option name.
    #[serde(rename = "opt")]
    Optional {
        /// Morloc serialization schema for the option's value type.
        #[serde(default)]
        schema: Option<String>,
        /// User-facing type name. JSON key is `type`.
        #[serde(default, rename = "type")]
        type_desc: Option<String>,
        /// Required display placeholder (e.g. `"FILE"`).
        #[serde(default)]
        metavar: Option<String>,
        /// JSON-wrap flag for `Str`-typed literal options.
        #[serde(default)]
        quoted: bool,
        /// Single-character short option (e.g. `"f"` for `-f`).
        #[serde(default, rename = "short")]
        short_opt: Option<String>,
        /// Long option name (e.g. `"file"` for `--file`).
        #[serde(default, rename = "long")]
        long_opt: Option<String>,
        /// Default value used when the user does not pass the option.
        /// Always present (declared via `--' default:` docstring).
        #[serde(default, rename = "default")]
        default_val: Option<String>,
        /// Description lines.
        #[serde(default)]
        desc: Vec<String>,
        /// Per-argument constraints -- see [`Arg::Positional`]'s
        /// `constraints` field.
        #[serde(default)]
        constraints: Vec<Constraint>,
        /// **Reserved.** Per-argument metadata. Not read in v2.
        #[serde(default)]
        metadata: Metadata,
    },
    /// A pure boolean flag toggle. Carries no type, schema, or
    /// constraints because it has no payload -- flipping the flag
    /// produces the value `true` or `false`.
    #[serde(rename = "flag")]
    Flag {
        /// Single-character short option (e.g. `"v"` for `-v`).
        #[serde(default, rename = "short")]
        short_opt: Option<String>,
        /// Long option name (e.g. `"verbose"` for `--verbose`).
        #[serde(default, rename = "long")]
        long_opt: Option<String>,
        /// Long option name that flips the flag in the opposite
        /// direction (e.g. `"no-verbose"` for `--no-verbose`).
        #[serde(default)]
        long_rev: Option<String>,
        /// Default value when the flag is not present on the CLI.
        /// String form: `"true"` or `"false"`.
        #[serde(default, rename = "default")]
        default_val: Option<String>,
        /// Description lines.
        #[serde(default)]
        desc: Vec<String>,
        /// **Reserved.** Per-flag metadata. Not read in v2.
        #[serde(default)]
        metadata: Metadata,
    },
    /// A record-typed argument that has been "unrolled" into a flat
    /// collection of CLI flags/options, one per record field. The
    /// group's top-level `schema` is the schema of the whole record;
    /// dispatch sends the assembled record to the pool, so individual
    /// entries never need their own schemas.
    #[serde(rename = "grp")]
    Group {
        /// Morloc schema for the whole record (a `Map` schema).
        #[serde(default)]
        schema: Option<String>,
        /// User-facing record type name (e.g. `"SysConfig"`).
        #[serde(default, rename = "type")]
        type_desc: Option<String>,
        /// Display placeholder for the group as a whole.
        #[serde(default)]
        metavar: Option<String>,
        /// Description lines for the group.
        #[serde(default)]
        desc: Vec<String>,
        /// Optional CLI option that accepts the entire record as a
        /// single JSON value (e.g. `--sys-config '{...}'`).
        #[serde(default)]
        group_opt: Option<GroupOpt>,
        /// Flattened per-field options/flags. Each entry's `arg` is a
        /// nested [`Arg`] (typically `Optional` or `Flag`) that has
        /// no schema of its own -- only the group's top-level schema
        /// matters at dispatch time.
        #[serde(default)]
        entries: Vec<GroupEntry>,
        /// Per-group constraints. Currently the `kind` constraint
        /// (almost always `record`) is emitted.
        #[serde(default)]
        constraints: Vec<Constraint>,
        /// **Reserved.** Per-group metadata. Not read in v2.
        #[serde(default)]
        metadata: Metadata,
    },
}

/// Nested CLI option that accepts the entire record (associated with
/// an [`Arg::Group`]) as a single JSON value.
#[derive(Debug, Deserialize)]
#[allow(dead_code)]
pub struct GroupOpt {
    /// Short option char that accepts the whole record as one JSON value.
    #[serde(default, rename = "short")]
    pub short_opt: Option<String>,
    /// Long option name that accepts the whole record as one JSON value.
    #[serde(default, rename = "long")]
    pub long_opt: Option<String>,
}

/// One entry inside an [`Arg::Group`] -- pairs a record field name
/// with the CLI flag/option that backs it.
#[derive(Debug, Deserialize)]
#[allow(dead_code)]
pub struct GroupEntry {
    /// Record field name.
    pub key: String,
    /// CLI binding for this field. Always a [`Arg::Optional`] or
    /// [`Arg::Flag`] in practice; never carries its own schema (the
    /// containing group's schema covers all fields).
    pub arg: Arg,
}

/// CLI command group -- purely organizational metadata used to bucket
/// related subcommands together in the help output.
#[derive(Debug, Deserialize)]
#[allow(dead_code)]
pub struct CmdGroup {
    /// Group name; matches `Command::group` on member commands.
    pub name: String,
    /// Group description lines for the help output.
    #[serde(default)]
    pub desc: Vec<String>,
    /// **Reserved.** Per-group metadata. Not read in v2.
    #[serde(default)]
    pub metadata: Metadata,
}

/// Daemon-mode service configuration. Present only when the program
/// is configured to run as a long-lived service rather than a one-shot
/// CLI invocation.
#[derive(Debug, Deserialize)]
#[allow(dead_code)]
pub struct Service {
    /// Transport type: typically `"http"`, `"tcp"`, or `"unix"`.
    #[serde(rename = "type")]
    pub service_type: Option<String>,
    /// Listening host address (TCP/HTTP).
    pub host: Option<String>,
    /// Listening port (TCP/HTTP).
    pub port: Option<i32>,
    /// Unix socket path (when `service_type` is `"unix"`).
    pub socket: Option<String>,
    /// **Reserved.** Per-service metadata. Not read in v2.
    #[serde(default)]
    pub metadata: Metadata,
}

// -- I/O ----------------------------------------------------------------------

/// Read the manifest payload from a built-nexus wrapper script. The
/// nexus binary is wrapped in a shell script that contains a
/// `### MANIFEST ###` marker followed by the JSON blob. Plain JSON
/// files (no shebang) are returned as-is.
pub fn read_manifest_payload(path: &str) -> Result<String, String> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| format!("Cannot open manifest file '{}': {}", path, e))?;

    if content.starts_with("#!") {
        if let Some(pos) = content.find("### MANIFEST ###") {
            let after_marker = &content[pos..];
            let payload_start = after_marker
                .find('\n')
                .map(|i| pos + i + 1)
                .unwrap_or(content.len());
            Ok(content[payload_start..].to_string())
        } else {
            Err("No ### MANIFEST ### marker found in wrapper script".into())
        }
    } else {
        Ok(content)
    }
}

/// Parse a manifest JSON payload into a [`Manifest`]. Performs a
/// staleness check on `build.morloc_version` against this crate's own
/// `CARGO_PKG_VERSION` (which is intentionally pinned to match the
/// morloc compiler version). Mismatched versions return a clean
/// "rebuild with the current compiler" error rather than silently
/// misinterpreting the manifest.
pub fn parse_manifest(payload: &str) -> Result<Manifest, String> {
    let m: Manifest = serde_json::from_str(payload)
        .map_err(|e| format!("Failed to parse manifest JSON: {}", e))?;
    let crate_version = env!("CARGO_PKG_VERSION");
    if m.build.morloc_version != crate_version {
        return Err(format!(
            "manifest built with morloc {}, runtime is {}; rebuild with the current compiler",
            m.build.morloc_version, crate_version
        ));
    }
    Ok(m)
}

// -- Arg accessors ------------------------------------------------------------
//
// Variant-agnostic helpers for the fields that exist on multiple Arg
// variants. Callers in the nexus and runtime use these instead of
// pattern-matching at every site.

impl Arg {
    /// Single-character short option (e.g. `'f'` for `-f`). Returns
    /// None for positional and group args.
    pub fn short_opt_char(&self) -> Option<char> {
        let s = match self {
            Arg::Optional { short_opt, .. } => short_opt.as_deref(),
            Arg::Flag { short_opt, .. } => short_opt.as_deref(),
            _ => None,
        };
        s.and_then(|s| s.chars().next())
    }

    /// Long option name (e.g. `"verbose"` for `--verbose`). Returns
    /// None for positional and group args.
    pub fn long_opt_str(&self) -> Option<&str> {
        match self {
            Arg::Optional { long_opt, .. } => long_opt.as_deref(),
            Arg::Flag { long_opt, .. } => long_opt.as_deref(),
            _ => None,
        }
    }

    /// True if this arg is a boolean flag toggle.
    pub fn is_flag(&self) -> bool {
        matches!(self, Arg::Flag { .. })
    }

    /// True if the user's CLI value should be JSON-wrapped before
    /// being passed to the pool. Used for `Str`-typed arguments
    /// declared with `--' literal: true`. Always false for flags and
    /// groups.
    pub fn is_quoted(&self) -> bool {
        match self {
            Arg::Positional { quoted, .. } | Arg::Optional { quoted, .. } => *quoted,
            _ => false,
        }
    }

    /// Default CLI value when the user does not pass the argument.
    /// Returns None for positional args (which are always required)
    /// and groups.
    pub fn default_val(&self) -> Option<&str> {
        match self {
            Arg::Optional { default_val, .. } => default_val.as_deref(),
            Arg::Flag { default_val, .. } => default_val.as_deref(),
            _ => None,
        }
    }

    /// CLI display placeholder (e.g. `"FILE"`, `"INT"`). None for
    /// flags and for positional args without an explicit metavar.
    pub fn metavar_str(&self) -> Option<&str> {
        match self {
            Arg::Positional { metavar, .. } => metavar.as_deref(),
            Arg::Optional { metavar, .. } => metavar.as_deref(),
            Arg::Group { metavar, .. } => metavar.as_deref(),
            _ => None,
        }
    }

    /// Description lines from the source-level docstring. Always
    /// available regardless of variant.
    pub fn desc_lines(&self) -> &[String] {
        match self {
            Arg::Positional { desc, .. }
            | Arg::Optional { desc, .. }
            | Arg::Flag { desc, .. }
            | Arg::Group { desc, .. } => desc,
        }
    }

    /// User-facing type name for typed args (e.g. `"Int"`,
    /// `"Config"`). Returns None for flags, which carry no type.
    pub fn type_desc_str(&self) -> Option<&str> {
        match self {
            Arg::Positional { type_desc, .. }
            | Arg::Optional { type_desc, .. }
            | Arg::Group { type_desc, .. } => type_desc.as_deref(),
            Arg::Flag { .. } => None,
        }
    }

    /// Morloc serialization schema string for typed args. Returns
    /// None for flags. The schema drives both dispatch (how to encode
    /// the value into a packet) and help rendering (how to extract
    /// record field layouts for the Record/Table Schemas sections).
    pub fn schema_str(&self) -> Option<&str> {
        match self {
            Arg::Positional { schema, .. }
            | Arg::Optional { schema, .. }
            | Arg::Group { schema, .. } => schema.as_deref(),
            Arg::Flag { .. } => None,
        }
    }

    /// All constraints attached to this arg. Empty for flags. The
    /// caller is responsible for filtering by constraint type and
    /// silently ignoring unknown types (per the extensibility rules
    /// on [`Constraint`]).
    pub fn constraints(&self) -> &[Constraint] {
        match self {
            Arg::Positional { constraints, .. }
            | Arg::Optional { constraints, .. }
            | Arg::Group { constraints, .. } => constraints,
            Arg::Flag { .. } => &[],
        }
    }

    /// Convenience accessor: extract the `value` of the `kind`
    /// constraint as a string slice. Returns the lowercased
    /// `"record"`, `"object"`, or `"table"` for named-type args.
    /// None for everything else (including untagged primitive types).
    pub fn kind_constraint(&self) -> Option<&str> {
        self.constraints()
            .iter()
            .find(|c| c.ctype == "kind")
            .and_then(|c| c.value.as_ref().and_then(|v| v.as_str()))
    }
}

// -- Tests --------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    /// Wrap a v2 command body with the required top-level fields so
    /// each test fixture stays compact.
    fn wrap(commands_json: &str) -> String {
        let v = env!("CARGO_PKG_VERSION");
        format!(
            r#"{{
                "name": "main",
                "build": {{
                    "path": "/tmp/test",
                    "time": 0,
                    "morloc_version": "{}"
                }},
                "pools": [
                    {{"lang": "py", "exec": ["python3", "pool.py"], "socket": "pipe-py", "metadata": {{}}}}
                ],
                "commands": {},
                "groups": [],
                "metadata": {{}}
            }}"#,
            v, commands_json
        )
    }

    #[test]
    fn test_parse_simple_manifest() {
        let json = wrap(
            r#"[
                {
                    "name": "f",
                    "type": "remote",
                    "mid": 1,
                    "pool": 0,
                    "needed_pools": [0],
                    "desc": [],
                    "args": [
                        {
                            "kind": "pos",
                            "schema": "s",
                            "type": "Str",
                            "metavar": null,
                            "quoted": false,
                            "desc": [],
                            "constraints": [],
                            "metadata": {}
                        }
                    ],
                    "return": {
                        "schema": "s",
                        "type": "Str",
                        "desc": [],
                        "constraints": [],
                        "metadata": {}
                    },
                    "constraints": [],
                    "metadata": {},
                    "group": null
                }
            ]"#,
        );
        let m = parse_manifest(&json).unwrap();
        assert_eq!(m.pools.len(), 1);
        assert_eq!(m.pools[0].lang, "py");
        assert_eq!(m.commands.len(), 1);
        assert_eq!(m.commands[0].name, "f");
        assert!(!m.commands[0].is_pure());
        assert_eq!(m.commands[0].mid, 1);
        assert!(m.commands[0].group.is_none());
        assert_eq!(m.commands[0].args.len(), 1);
        assert_eq!(m.commands[0].args[0].schema_str(), Some("s"));
        assert_eq!(m.commands[0].args[0].type_desc_str(), Some("Str"));
        assert_eq!(m.commands[0].ret.schema, "s");
        assert_eq!(m.commands[0].ret.type_desc, "Str");
    }

    #[test]
    fn test_parse_pure_command() {
        let json = wrap(
            r#"[
                {
                    "name": "greet",
                    "type": "pure",
                    "desc": ["Say hello"],
                    "args": [
                        {
                            "kind": "pos",
                            "schema": "s",
                            "type": "Str",
                            "metavar": "NAME",
                            "quoted": true,
                            "desc": ["name"],
                            "constraints": [],
                            "metadata": {}
                        }
                    ],
                    "return": {
                        "schema": "s",
                        "type": "Str",
                        "desc": [],
                        "constraints": [],
                        "metadata": {}
                    },
                    "expr": {"tag": "lit", "schema": "s", "lit_type": "str", "value": "hello"},
                    "constraints": [],
                    "metadata": {},
                    "group": null
                }
            ]"#,
        );
        let m = parse_manifest(&json).unwrap();
        assert!(m.commands[0].is_pure());
        assert!(m.commands[0].expr.is_some());
    }

    #[test]
    fn test_parse_kind_constraint() {
        let json = wrap(
            r#"[
                {
                    "name": "process",
                    "type": "remote",
                    "mid": 1,
                    "pool": 0,
                    "needed_pools": [0],
                    "desc": [],
                    "args": [
                        {
                            "kind": "pos",
                            "schema": "<dict>m24name<list>a<str>s3age<list>a<int>i4",
                            "type": "People",
                            "metavar": null,
                            "quoted": false,
                            "desc": [],
                            "constraints": [
                                {"type": "kind", "value": "table"}
                            ],
                            "metadata": {}
                        }
                    ],
                    "return": {
                        "schema": "i4",
                        "type": "Int",
                        "desc": [],
                        "constraints": [],
                        "metadata": {}
                    },
                    "constraints": [],
                    "metadata": {},
                    "group": null
                }
            ]"#,
        );
        let m = parse_manifest(&json).unwrap();
        assert_eq!(m.commands[0].args[0].kind_constraint(), Some("table"));
    }

    #[test]
    fn test_version_mismatch_rejected() {
        let json = r#"{
            "name": "main",
            "build": {"path": "/tmp/x", "time": 0, "morloc_version": "0.0.1-stale"},
            "pools": [],
            "commands": [],
            "groups": [],
            "metadata": {}
        }"#;
        let err = parse_manifest(json).unwrap_err();
        assert!(
            err.contains("rebuild with the current compiler"),
            "got: {}",
            err
        );
    }
}
