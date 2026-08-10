//! Machine-readable help emitters for compiled morloc programs.
//!
//! Two projections of the loaded [`morloc_manifest::Manifest`]:
//!
//! * [`print_json_help`] -- a lossless, morloc-native JSON view of the same
//!   information the human `--help` renders: positional/option/flag/group
//!   roles, metavars, the `source:`/`form:`/`check.*`/`list.*` CLI-shape
//!   grammar, variadic/stdin/quoted, defaults, terminal-action formatter
//!   flags, morloc type strings + wire schemas, record/table field layouts,
//!   return info, and module/command docstrings. "morloc help, but
//!   structured."
//!
//! * [`print_mcp_tools`] -- a lossy compatibility view into the MCP tool
//!   schema: a `tools[]` array whose members are `{name, description,
//!   inputSchema, outputSchema}` with JSON Schema types. This is the payload
//!   an MCP client's `tools/list` returns.
//!
//! Both read the manifest only; neither spawns pools nor touches shared
//! memory. The `--json-help` / `--mcp-tools` nexus flags reserve those
//! spellings against a program's own declared flags, the same way `-h` is
//! reserved.

use serde_json::{json, Map, Value};

use morloc_manifest::{Arg, Check, Command, FormAtom, Manifest, SourceAtom};
use morloc_runtime_types::schema::{parse_schema, Schema, SerialType};

use crate::schemas::render_schema_type;

// ---------------------------------------------------------------------------
// Public entry points
// ---------------------------------------------------------------------------

/// Print the lossless morloc-native JSON help for a program to stdout.
pub fn print_json_help(m: &Manifest) {
    let v = build_json_help(m);
    println!("{}", serde_json::to_string_pretty(&v).unwrap());
}

/// Print the MCP `tools/list` payload for a program to stdout.
pub fn print_mcp_tools(m: &Manifest) {
    let v = build_mcp_tools(m);
    println!("{}", serde_json::to_string_pretty(&v).unwrap());
}

// ---------------------------------------------------------------------------
// Wire schema -> JSON Schema
// ---------------------------------------------------------------------------

/// Convert a parsed wire [`Schema`] into a JSON Schema value. Sibling to
/// [`crate::schemas::render_schema_type`]; walks the same tree.
fn schema_to_json_schema(s: &Schema) -> Value {
    use SerialType::*;
    match s.serial_type {
        Nil => json!({ "type": "null" }),
        Bool => json!({ "type": "boolean" }),
        Sint8 | Sint16 | Sint32 | Sint64 | Uint8 | Uint16 | Uint32 | Uint64 | Int => {
            json!({ "type": "integer" })
        }
        Float32 | Float64 => json!({ "type": "number" }),
        String => json!({ "type": "string" }),
        Array => {
            let items = s
                .parameters
                .first()
                .map(schema_to_json_schema)
                .unwrap_or_else(|| json!({}));
            json!({ "type": "array", "items": items })
        }
        Tuple => {
            let prefix: Vec<Value> =
                s.parameters.iter().map(schema_to_json_schema).collect();
            let n = prefix.len();
            json!({
                "type": "array",
                "prefixItems": prefix,
                "minItems": n,
                "maxItems": n
            })
        }
        Map => object_schema(&s.keys, &s.parameters),
        Table => {
            // Each column's wire schema is an Array; peel one layer so the row
            // object shows the element type, mirroring how the user wrote it.
            let cols: Vec<Schema> = s
                .parameters
                .iter()
                .map(|p| {
                    if p.serial_type == Array {
                        p.parameters.first().cloned().unwrap_or_else(|| p.clone())
                    } else {
                        p.clone()
                    }
                })
                .collect();
            json!({ "type": "array", "items": object_schema(&s.keys, &cols) })
        }
        Optional => {
            // Nullability is expressed both by the parent dropping this from
            // `required` and by unioning "null" into the type here, so the
            // schema is self-describing when read in isolation.
            let inner = s
                .parameters
                .first()
                .map(schema_to_json_schema)
                .unwrap_or_else(|| json!({}));
            with_null(inner)
        }
        // Back-reference into a recursive record: do not expand (would not
        // terminate). Surface the object shape without its fields.
        Recur => json!({ "type": "object" }),
        // Stream handles surface as their path/handle string form.
        IFile | OStream | IStream => json!({ "type": "string" }),
    }
}

/// Build a JSON Schema object from parallel key/field-schema slices. A field
/// is required unless its own schema is `Optional`.
fn object_schema(keys: &[std::string::String], fields: &[Schema]) -> Value {
    let mut props = Map::new();
    let mut required: Vec<Value> = Vec::new();
    for (i, field) in fields.iter().enumerate() {
        let key = keys
            .get(i)
            .cloned()
            .unwrap_or_else(|| format!("field{}", i));
        if field.serial_type != SerialType::Optional {
            required.push(Value::String(key.clone()));
        }
        props.insert(key, schema_to_json_schema(field));
    }
    json!({
        "type": "object",
        "properties": Value::Object(props),
        "required": required,
        "additionalProperties": false
    })
}

/// Union `"null"` into an object schema's `type` field.
fn with_null(mut v: Value) -> Value {
    if let Value::Object(ref mut m) = v {
        match m.get("type").cloned() {
            Some(Value::String(t)) => {
                m.insert("type".into(), json!([t, "null"]));
            }
            Some(Value::Array(mut arr)) => {
                if !arr.iter().any(|x| x == "null") {
                    arr.push(Value::String("null".into()));
                }
                m.insert("type".into(), Value::Array(arr));
            }
            _ => {}
        }
    }
    v
}

/// True when a wire schema string denotes a top-level optional type.
fn schema_is_optional(schema: Option<&str>) -> bool {
    schema
        .and_then(|s| parse_schema(s).ok())
        .map(|p| p.serial_type == SerialType::Optional)
        .unwrap_or(false)
}

// ---------------------------------------------------------------------------
// --json-help: lossless morloc-native view
// ---------------------------------------------------------------------------

fn build_json_help(m: &Manifest) -> Value {
    let commands: Vec<Value> = m
        .commands
        .iter()
        .filter(|c| !c.internal)
        .map(command_to_json)
        .collect();
    let groups: Vec<Value> = m
        .groups
        .iter()
        .map(|g| json!({ "name": g.name, "description": g.desc }))
        .collect();
    json!({
        "morloc_version": m.build.morloc_version,
        "program": {
            "name": m.name,
            "description": m.desc,
        },
        "groups": groups,
        "commands": commands,
    })
}

fn command_to_json(cmd: &Command) -> Value {
    let mut arguments: Vec<Value> = Vec::with_capacity(cmd.args.len());
    let mut pos_index = 0usize;
    for arg in &cmd.args {
        arguments.push(arg_to_json(arg, pos_index));
        if matches!(arg, Arg::Positional { .. }) {
            pos_index += 1;
        }
    }

    let terminals: Vec<Value> = cmd
        .terminals
        .iter()
        .map(|t| {
            json!({
                "short": t.short.map(|c| c.to_string()),
                "long": t.long,
                "description": t.description,
                "render": t.render,
                "default": t.default,
            })
        })
        .collect();

    let ret_schema = non_empty(&cmd.ret.schema);
    let ret_type = non_empty(&cmd.ret.type_desc);

    json!({
        "name": cmd.name,
        "kind": if cmd.is_pure() { "pure" } else { "remote" },
        "group": cmd.group,
        "description": cmd.desc,
        "arguments": arguments,
        "return": {
            "description": cmd.ret.desc,
            "type": type_object(ret_schema, ret_type),
        },
        "terminals": terminals,
    })
}

/// Render one manifest [`Arg`] to its lossless JSON object. `pos_index` is the
/// zero-based position among positional args (ignored for non-positionals).
fn arg_to_json(arg: &Arg, pos_index: usize) -> Value {
    match arg {
        Arg::Positional {
            schema,
            type_desc,
            metavar,
            quoted,
            many,
            stdin,
            desc,
            source,
            form,
            checks,
            list_source,
            list_form,
            list_checks,
            format,
            ..
        } => {
            let name = metavar
                .as_deref()
                .map(|m| m.to_lowercase())
                .unwrap_or_else(|| format!("arg{}", pos_index));
            json!({
                "name": name,
                "role": "positional",
                "position": pos_index,
                "metavar": metavar,
                "required": !schema_is_optional(schema.as_deref()) && !*stdin,
                "variadic": many,
                "stdin": stdin,
                "quoted": quoted,
                "default": Value::Null,
                "description": desc,
                "type": type_object(schema.as_deref(), type_desc.as_deref()),
                "named_type_kind": arg.kind_constraint(),
                "input": input_object(
                    source, form, checks, list_source, list_form, list_checks,
                    format.as_deref(),
                ),
            })
        }
        Arg::Optional {
            schema,
            type_desc,
            metavar,
            quoted,
            many,
            short_opt,
            long_opt,
            default_val,
            desc,
            source,
            form,
            checks,
            list_source,
            list_form,
            list_checks,
            format,
            ..
        } => {
            let name = opt_name(long_opt.as_deref(), short_opt.as_deref());
            json!({
                "name": name,
                "role": "option",
                "metavar": metavar,
                "required": false,
                "variadic": many,
                "quoted": quoted,
                "short": short_opt,
                "long": long_opt,
                "default": default_val,
                "description": desc,
                "type": type_object(schema.as_deref(), type_desc.as_deref()),
                "named_type_kind": arg.kind_constraint(),
                "input": input_object(
                    source, form, checks, list_source, list_form, list_checks,
                    format.as_deref(),
                ),
            })
        }
        Arg::Flag {
            short_opt,
            long_opt,
            long_rev,
            default_val,
            desc,
            ..
        } => {
            let name = opt_name(long_opt.as_deref(), short_opt.as_deref());
            json!({
                "name": name,
                "role": "flag",
                "required": false,
                "short": short_opt,
                "long": long_opt,
                "long_reverse": long_rev,
                "default": default_val,
                "description": desc,
                "type": { "morloc": "Bool", "structure": { "type": "boolean" } },
            })
        }
        Arg::Group {
            schema,
            type_desc,
            metavar,
            desc,
            group_opt,
            entries,
            ..
        } => {
            let name = type_desc
                .as_deref()
                .map(|t| t.to_lowercase())
                .unwrap_or_else(|| "group".to_string());
            let entries_json: Vec<Value> = entries
                .iter()
                .map(|e| json!({ "key": e.key, "argument": arg_to_json(&e.arg, 0) }))
                .collect();
            let group_option = group_opt.as_ref().map(|g| {
                json!({ "short": g.short_opt, "long": g.long_opt })
            });
            json!({
                "name": name,
                "role": "group",
                "metavar": metavar,
                "required": true,
                "description": desc,
                "type": type_object(schema.as_deref(), type_desc.as_deref()),
                "named_type_kind": arg.kind_constraint(),
                "group_option": group_option,
                "entries": entries_json,
            })
        }
    }
}

/// Build the `type` object: morloc type string, compact wire schema, and the
/// structured JSON Schema. Any component with no data is omitted.
fn type_object(schema: Option<&str>, type_desc: Option<&str>) -> Value {
    let mut obj = Map::new();
    let parsed = schema.and_then(|s| parse_schema(s).ok());
    // Prefer the manifest's user-facing type name (carries named types like
    // `Config`); fall back to rendering the parsed wire schema.
    let morloc = type_desc
        .map(|t| t.to_string())
        .or_else(|| parsed.as_ref().map(|p| render_schema_type(p, None)));
    if let Some(t) = morloc {
        obj.insert("morloc".into(), Value::String(t));
    }
    if let Some(sc) = schema {
        obj.insert("wire".into(), Value::String(sc.to_string()));
    }
    if let Some(p) = parsed.as_ref() {
        obj.insert("structure".into(), schema_to_json_schema(p));
    }
    Value::Object(obj)
}

/// The `source:`/`form:`/`check.*`/`list.*` CLI-shape grammar as JSON.
fn input_object(
    source: &SourceAtom,
    form: &FormAtom,
    checks: &[Check],
    list_source: &SourceAtom,
    list_form: &FormAtom,
    list_checks: &[Check],
    format: Option<&str>,
) -> Value {
    json!({
        "source": source_str(source),
        "form": form_str(form),
        "checks": checks_json(checks),
        "list_source": source_str(list_source),
        "list_form": form_str(list_form),
        "list_checks": checks_json(list_checks),
        "format": format,
    })
}

fn source_str(s: &SourceAtom) -> &'static str {
    match s {
        SourceAtom::Auto => "auto",
        SourceAtom::Inline => "inline",
        SourceAtom::File => "file",
    }
}

fn form_str(f: &FormAtom) -> &'static str {
    match f {
        FormAtom::Auto => "auto",
        FormAtom::Packet => "packet",
        FormAtom::Bytes => "bytes",
        FormAtom::BytesOnly => "bytes-only",
        FormAtom::List => "list",
    }
}

fn checks_json(checks: &[Check]) -> Value {
    Value::Array(
        checks
            .iter()
            .map(|c| match c {
                Check::Path(p) => json!({ "kind": "path", "value": p }),
            })
            .collect(),
    )
}

/// Synthesize a parameter name for an option/flag: long form, else short.
fn opt_name(long: Option<&str>, short: Option<&str>) -> String {
    long.map(|s| s.to_string())
        .or_else(|| short.map(|s| s.to_string()))
        .unwrap_or_else(|| "option".to_string())
}

fn non_empty(s: &str) -> Option<&str> {
    if s.is_empty() {
        None
    } else {
        Some(s)
    }
}

// ---------------------------------------------------------------------------
// --mcp-tools: MCP tools/list payload + tools/call inverse mapping
// ---------------------------------------------------------------------------
//
// One traversal per command builds BOTH the forward MCP `inputSchema` and the
// inverse `ArgSlot` list that reconstructs the positional args array
// `daemon_dispatch` requires. Keeping them in a single source of truth is what
// guarantees the advertised property names and the reader agree.

/// How one positional slot in the `daemon_dispatch` args array is recovered
/// from the MCP named `arguments` object. There is exactly one slot per
/// `Command::args` entry, in declaration order (Group / Flag / Optional /
/// Positional each occupy one slot).
#[derive(Debug, Clone)]
pub enum ArgSlot {
    /// A single value read from `arguments[key]`; when absent, `missing` is
    /// substituted. (Required-ness is enforced once, up front, against
    /// `McpToolShape::required` -- see `resolve_arguments` -- so it is not
    /// repeated per slot here.)
    Value {
        key: String,
        missing: Value,
    },
    /// A boolean flag read from `arguments[key]`; `default` when absent.
    Flag { key: String, default: bool },
    /// A record argument. In the unrolled form (`group_key` is None) each field
    /// is read from `arguments[field.field]`. In the whole-object form
    /// (`group_key` is Some) the client may instead pass the entire record
    /// under that one key; when it is absent the record is assembled from the
    /// per-field defaults.
    Record {
        group_key: Option<String>,
        fields: Vec<RecordField>,
    },
}

/// One field of a record `ArgSlot`, with the default used when the client
/// omits it.
#[derive(Debug, Clone)]
pub struct RecordField {
    pub field: String,
    pub default: Value,
}

/// One selectable output projection of a command, exposed via the synthetic
/// `render` enum. `"raw"` (the command's own typed value) is implicit and not
/// listed here; each entry is a `@render`/`@with` terminal.
pub struct RenderTarget {
    /// The `render` enum value (the terminal's long flag name, e.g. `"png"`).
    pub value: String,
    /// The internal command dispatched when this projection is selected (the
    /// terminal's mangled `entry`).
    pub command: String,
    /// Media type of that command's return, from its `@mime` type. Drives the
    /// MCP content block (`image/*` -> image block, etc.). Filled by
    /// [`build_tool_shapes`] from the manifest; `None` when untyped.
    pub mime: Option<String>,
    /// True when the projection's return is a Map/record.
    pub returns_object: bool,
}

/// The complete MCP shape of one command: the `tools/list` entry plus the
/// inverse mapping and validation metadata needed to service a `tools/call`.
pub struct McpToolShape {
    pub name: String,
    /// The MCP tool object for `tools/list`.
    pub tool: Value,
    /// One slot per `Command::args`, in declaration order.
    pub slots: Vec<ArgSlot>,
    /// Every property name the `inputSchema` advertises (for
    /// unknown-argument rejection).
    pub prop_names: std::collections::HashSet<String>,
    /// Property names that must be present (for missing-argument rejection).
    pub required: Vec<String>,
    /// True when the return type is a Map/record: only then do we emit
    /// `structuredContent` + an object `outputSchema`.
    pub returns_object: bool,
    /// Media type of the command's own (`"raw"`) return, from its `@mime` type;
    /// `None` when untyped. Used when no renderer is selected.
    pub return_mime: Option<String>,
    /// Output projections selectable via the `render` enum (empty when the
    /// command has no terminals).
    pub renders: Vec<RenderTarget>,
}

impl McpToolShape {
    /// Resolve a `render` selection to the command to dispatch and how to
    /// package its result: `"raw"` (the default) -> this command itself; a
    /// renderer name -> that terminal's entry command (dispatched with the same
    /// positional args). `Err` for an unknown renderer.
    pub fn dispatch_for(&self, render: &str) -> Result<(&str, Option<&str>, bool), String> {
        if render == "raw" {
            return Ok((
                self.name.as_str(),
                self.return_mime.as_deref(),
                self.returns_object,
            ));
        }
        self.renders
            .iter()
            .find(|r| r.value == render)
            .map(|t| (t.command.as_str(), t.mime.as_deref(), t.returns_object))
            .ok_or_else(|| format!("unknown render '{}'", render))
    }
}

/// True when a return's parsed wire schema is a Map/record -- the single
/// condition under which MCP emits `structuredContent` + an object
/// `outputSchema`.
fn schema_returns_object(p: &Schema) -> bool {
    p.serial_type == SerialType::Map
}

/// True when a wire schema tree contains a type the MCP tool surface cannot
/// marshal or safely serve: Arrow `Table` (marshaling errors both directions) or
/// a stream handle (`IFile`/`IStream`/`OStream`, which would need per-call
/// stdout capture). Checked on every argument and the return type.
fn schema_tree_excluded(s: &Schema) -> bool {
    use SerialType::*;
    matches!(s.serial_type, Table | IFile | IStream | OStream)
        || s.parameters.iter().any(schema_tree_excluded)
}

fn schema_str_excluded(schema: Option<&str>) -> bool {
    schema
        .and_then(|s| parse_schema(s).ok())
        .map(|p| schema_tree_excluded(&p))
        .unwrap_or(false)
}

/// Strip one layer of surrounding morloc string quotes from a CLI default
/// (e.g. the manifest stores a `Str` default `/tmp` as the literal `"/tmp"`).
fn strip_cli_quotes(s: &str) -> String {
    let b = s.as_bytes();
    if b.len() >= 2 && b[0] == b'"' && b[b.len() - 1] == b'"' {
        s[1..s.len() - 1].to_string()
    } else {
        s.to_string()
    }
}

/// Convert a CLI-shaped default string into a typed JSON value guided by the
/// value's wire schema. Manifest defaults are always CLI strings; the MCP
/// dispatch path feeds typed JSON to `daemon_dispatch`, so `"1"` for an `Int`
/// must become the number `1`, not the string `"1"`. Falls back to a JSON
/// string when the schema is unknown or the parse fails.
fn cli_default_to_json(default_val: Option<&str>, st: Option<&Schema>) -> Value {
    use SerialType::*;
    let raw = match default_val {
        Some(s) if !s.is_empty() => s,
        _ => return Value::Null,
    };
    // Peel one Optional layer so `?Int` defaults type against `Int`.
    let eff = st.and_then(|s| {
        if s.serial_type == Optional {
            s.parameters.first()
        } else {
            Some(s)
        }
    });
    match eff.map(|s| s.serial_type) {
        Some(Bool) => match raw {
            "true" => json!(true),
            "false" => json!(false),
            _ => Value::String(raw.to_string()),
        },
        Some(Sint8 | Sint16 | Sint32 | Sint64 | Uint8 | Uint16 | Uint32 | Uint64 | Int) => raw
            .parse::<i64>()
            .map(|n| json!(n))
            .or_else(|_| raw.parse::<f64>().map(|f| json!(f)))
            .unwrap_or_else(|_| Value::String(raw.to_string())),
        Some(Float32 | Float64) => raw
            .parse::<f64>()
            .map(|f| json!(f))
            .unwrap_or_else(|_| Value::String(raw.to_string())),
        Some(String) => Value::String(strip_cli_quotes(raw)),
        _ => serde_json::from_str(raw).unwrap_or_else(|_| Value::String(raw.to_string())),
    }
}

/// Build the record fields (name + typed default) for an [`Arg::Group`], using
/// the group's Map schema to type each field's default. Shared by both the
/// unrolled and whole-object group forms.
fn group_record_fields(
    entries: &[morloc_manifest::GroupEntry],
    map: Option<&Schema>,
) -> Vec<RecordField> {
    entries
        .iter()
        .map(|entry| {
            let field_schema = map.and_then(|m| {
                m.keys
                    .iter()
                    .position(|k| k == &entry.key)
                    .and_then(|i| m.parameters.get(i))
            });
            let default = match &entry.arg {
                Arg::Flag { default_val, .. } => {
                    Value::Bool(default_val.as_deref() == Some("true"))
                }
                Arg::Optional { default_val, .. } => {
                    cli_default_to_json(default_val.as_deref(), field_schema)
                }
                _ => Value::Null,
            };
            RecordField {
                field: entry.key.clone(),
                default,
            }
        })
        .collect()
}

fn build_mcp_tools(m: &Manifest) -> Value {
    let tools: Vec<Value> = build_tool_shapes(m).into_iter().map(|s| s.tool).collect();
    json!({ "tools": tools })
}

/// Non-internal commands servable over the JSON/packet wire, in manifest
/// order. Applies the same exclusions as [`build_tool_shapes`] (`@stdin`,
/// Arrow `Table` / stream-handle types, property-name collisions) but emits
/// no stderr note, so the daemon/mcp `-h` help renderer can list the servable
/// surface without printing exclusion warnings.
pub fn servable_commands(m: &Manifest) -> Vec<&Command> {
    m.commands
        .iter()
        .filter(|c| !c.internal)
        .filter(|c| command_to_tool_shape(c).is_ok())
        .collect()
}

/// Build the MCP shape of every servable command. Commands the MCP tool surface
/// cannot serve (Arrow `Table` / stream-handle types, `@stdin`, or a
/// property-name collision) are dropped with a note on stderr.
pub fn build_tool_shapes(m: &Manifest) -> Vec<McpToolShape> {
    m.commands
        .iter()
        .filter(|c| !c.internal)
        .filter_map(|c| match command_to_tool_shape(c) {
            Ok(mut shape) => {
                fill_render_targets(&mut shape, m);
                Some(shape)
            }
            Err(reason) => {
                eprintln!(
                    "morloc mcp: excluding command '{}' from the tool surface ({})",
                    c.name, reason
                );
                None
            }
        })
        .collect()
}

/// Build one command's [`McpToolShape`], or `Err(reason)` if it must be
/// excluded from the MCP tool surface.
fn command_to_tool_shape(cmd: &Command) -> Result<McpToolShape, String> {
    // Exclusion gate (also a correctness precondition: a leaked streaming
    // command would write to a stdout fd the MCP loop has aliased away).
    for arg in &cmd.args {
        if let Arg::Positional { stdin: true, .. } = arg {
            return Err("reads from @stdin".into());
        }
        if schema_str_excluded(arg.schema_str()) {
            return Err("argument has an Arrow Table or stream-handle type".into());
        }
    }
    if schema_str_excluded(non_empty(&cmd.ret.schema)) {
        return Err("return has an Arrow Table or stream-handle type".into());
    }

    let mut props = Map::new();
    let mut required: Vec<Value> = Vec::new();
    let mut slots: Vec<ArgSlot> = Vec::with_capacity(cmd.args.len());
    let mut pos_index = 0usize;

    // Insert a property, failing closed on a name collision (the forward map
    // would otherwise silently overwrite, letting two args collapse to one
    // key with no inverse).
    let insert_prop = |props: &mut Map<String, Value>, name: &str, prop: Value| -> Result<(), String> {
        if props.contains_key(name) {
            return Err(format!("property name collision on '{}'", name));
        }
        props.insert(name.to_string(), prop);
        Ok(())
    };

    for arg in &cmd.args {
        match arg {
            Arg::Positional {
                schema,
                many,
                stdin,
                desc,
                ..
            } => {
                // Positionals get a reserved, collision-proof key `_<1-based
                // index>`. A metavar (FILE/INT/...) is a generic display
                // placeholder that gets reused across positionals and would
                // collide; the flag parser reserves the leading `_` so an
                // option can never produce this name. The positional's type and
                // `--' desc:` still ride in the property's schema + description.
                let name = format!("_{}", pos_index + 1);
                let mut prop = mcp_type(schema.as_deref(), *many);
                set_description(&mut prop, desc, None);
                let is_req = !schema_is_optional(schema.as_deref()) && !*stdin;
                if is_req {
                    required.push(Value::String(name.clone()));
                }
                insert_prop(&mut props, &name, prop)?;
                slots.push(ArgSlot::Value {
                    key: name,
                    missing: Value::Null,
                });
                pos_index += 1;
            }
            Arg::Optional {
                schema,
                many,
                short_opt,
                long_opt,
                default_val,
                desc,
                ..
            } => {
                let name = opt_name(long_opt.as_deref(), short_opt.as_deref());
                let mut prop = mcp_type(schema.as_deref(), *many);
                set_description(&mut prop, desc, default_val.as_deref());
                insert_prop(&mut props, &name, prop)?;
                let st = schema.as_deref().and_then(|s| parse_schema(s).ok());
                slots.push(ArgSlot::Value {
                    key: name,
                    missing: cli_default_to_json(default_val.as_deref(), st.as_ref()),
                });
            }
            Arg::Flag {
                short_opt,
                long_opt,
                default_val,
                desc,
                ..
            } => {
                let name = opt_name(long_opt.as_deref(), short_opt.as_deref());
                let mut prop = json!({ "type": "boolean" });
                set_description(&mut prop, desc, default_val.as_deref());
                insert_prop(&mut props, &name, prop)?;
                slots.push(ArgSlot::Flag {
                    key: name,
                    default: default_val.as_deref() == Some("true"),
                });
            }
            Arg::Group {
                schema,
                type_desc,
                group_opt,
                entries,
                desc,
                ..
            } => {
                let parsed = schema.as_deref().and_then(|s| parse_schema(s).ok());
                let fields = group_record_fields(entries, parsed.as_ref());
                if group_opt.is_some() {
                    // Whole record passed as one JSON object property. Not
                    // marked required: an all-defaulted record may be omitted.
                    let name = type_desc
                        .as_deref()
                        .map(|t| t.to_lowercase())
                        .unwrap_or_else(|| "group".to_string());
                    let mut prop = parsed
                        .as_ref()
                        .map(schema_to_json_schema)
                        .unwrap_or_else(|| json!({ "type": "object" }));
                    set_description(&mut prop, desc, None);
                    insert_prop(&mut props, &name, prop)?;
                    slots.push(ArgSlot::Record {
                        group_key: Some(name),
                        fields,
                    });
                } else {
                    // Unrolled: one property per field, keyed by field name. A
                    // field is required only if it is non-optional AND carries
                    // no default -- an unrolled field with a default (the usual
                    // case: every non-bool field needs one, every flag has one)
                    // is filled by the inverse when omitted, so advertising it
                    // as required would wrongly force clients to supply it.
                    if let Some(ref map) = parsed {
                        for entry in entries {
                            let field = map
                                .keys
                                .iter()
                                .position(|k| k == &entry.key)
                                .and_then(|i| map.parameters.get(i));
                            let mut prop = field
                                .map(schema_to_json_schema)
                                .unwrap_or_else(|| json!({}));
                            // Carry the field's docstring + default into the
                            // property description, mirroring a top-level option.
                            set_description(
                                &mut prop,
                                entry.arg.desc_lines(),
                                entry.arg.default_val(),
                            );
                            let has_default = entry.arg.default_val().is_some();
                            let is_req = !has_default
                                && field
                                    .map(|f| f.serial_type != SerialType::Optional)
                                    .unwrap_or(false);
                            if is_req {
                                required.push(Value::String(entry.key.clone()));
                            }
                            insert_prop(&mut props, &entry.key, prop)?;
                        }
                    }
                    slots.push(ArgSlot::Record {
                        group_key: None,
                        fields,
                    });
                }
            }
        }
    }

    // Output-projection selection. A command with `@render`/`@with` terminals
    // exposes each projection through a synthetic `render` enum on the parent
    // tool; `"raw"` (the default) returns the command's own typed value. The
    // per-target media type is filled from the manifest in `build_tool_shapes`.
    // `render` is a routing property, not a pool argument, so it gets NO
    // `ArgSlot` and is never part of the inverted positional array.
    let mut renders: Vec<RenderTarget> = Vec::new();
    if !cmd.terminals.is_empty() {
        let mut enum_vals: Vec<Value> = vec![Value::String("raw".into())];
        for t in &cmd.terminals {
            enum_vals.push(Value::String(t.long.clone()));
            renders.push(RenderTarget {
                value: t.long.clone(),
                command: t.entry.clone(),
                mime: None,
                returns_object: false,
            });
        }
        // MCP intentionally defaults to "raw" (the structured value), NOT the
        // CLI's `@default` renderer: an agent generally wants the typed data and
        // opts into a renderer via this enum, so `Terminal::default` is
        // deliberately not consulted here.
        let prop = json!({
            "type": "string",
            "enum": enum_vals,
            "default": "raw",
            "description":
                "Output projection: 'raw' returns the underlying typed value; a \
                 renderer name returns that projection (e.g. an image)."
        });
        insert_prop(&mut props, "render", prop)?;
    }

    let prop_names: std::collections::HashSet<String> = props.keys().cloned().collect();
    let required_names: Vec<String> = required
        .iter()
        .filter_map(|v| v.as_str().map(|s| s.to_string()))
        .collect();

    let ret_parsed = parse_schema(&cmd.ret.schema).ok();
    let returns_object = ret_parsed.as_ref().map(schema_returns_object).unwrap_or(false);

    let mut tool = Map::new();
    tool.insert("name".into(), Value::String(cmd.name.clone()));
    tool.insert("description".into(), Value::String(cmd.desc.join("\n")));
    tool.insert(
        "inputSchema".into(),
        json!({
            "type": "object",
            "properties": Value::Object(props),
            "required": required,
            "additionalProperties": false
        }),
    );
    // Output schema: emit ONLY for object (Map/record) returns. MCP
    // `structuredContent` must be a JSON object, so a scalar/array
    // `outputSchema` would be invalid; those results ride in a text block.
    if returns_object {
        if let Some(ref parsed) = ret_parsed {
            tool.insert("outputSchema".into(), schema_to_json_schema(parsed));
        }
    }

    Ok(McpToolShape {
        name: cmd.name.clone(),
        tool: Value::Object(tool),
        slots,
        prop_names,
        required: required_names,
        returns_object,
        return_mime: cmd.ret.mime.clone(),
        renders,
    })
}

/// Fill each render target's media type + object-ness by looking up its entry
/// command in the manifest. Split from `command_to_tool_shape` (which sees only
/// one command) so the shape builder stays manifest-free and unit-testable.
fn fill_render_targets(shape: &mut McpToolShape, m: &Manifest) {
    for rt in &mut shape.renders {
        if let Some(entry) = m.commands.iter().find(|c| c.name == rt.command) {
            rt.mime = entry.ret.mime.clone();
            rt.returns_object = parse_schema(&entry.ret.schema)
                .ok()
                .as_ref()
                .map(schema_returns_object)
                .unwrap_or(false);
        }
    }
}

/// JSON Schema type for a typed arg, wrapping in an array when variadic (but
/// never double-wrapping an already-array schema).
fn mcp_type(schema: Option<&str>, many: bool) -> Value {
    let base = schema
        .and_then(|s| parse_schema(s).ok())
        .map(|p| schema_to_json_schema(&p))
        .unwrap_or_else(|| json!({}));
    if many {
        let is_array = base.get("type").and_then(|t| t.as_str()) == Some("array");
        if is_array {
            base
        } else {
            json!({ "type": "array", "items": base })
        }
    } else {
        base
    }
}

/// Attach a `description` to a JSON Schema property from docstring lines and an
/// optional default value. No-op when both are empty.
fn set_description(prop: &mut Value, desc: &[std::string::String], default: Option<&str>) {
    let mut parts: Vec<std::string::String> =
        desc.iter().filter(|l| !l.is_empty()).cloned().collect();
    if let Some(d) = default {
        parts.push(format!("(default: {})", d));
    }
    if parts.is_empty() {
        return;
    }
    if let Value::Object(ref mut m) = prop {
        m.insert("description".into(), Value::String(parts.join(" ")));
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn js(schema: &str) -> Value {
        schema_to_json_schema(&parse_schema(schema).unwrap())
    }

    #[test]
    fn scalar_schemas() {
        assert_eq!(js("s"), json!({ "type": "string" }));
        assert_eq!(js("b"), json!({ "type": "boolean" }));
        assert_eq!(js("i4"), json!({ "type": "integer" }));
        assert_eq!(js("f8"), json!({ "type": "number" }));
    }

    #[test]
    fn array_schema() {
        assert_eq!(
            js("ai4"),
            json!({ "type": "array", "items": { "type": "integer" } })
        );
    }

    #[test]
    fn tuple_uses_prefix_items() {
        let v = js("t2f8f8");
        assert_eq!(v["type"], "array");
        assert_eq!(v["minItems"], 2);
        assert_eq!(v["maxItems"], 2);
        assert_eq!(v["prefixItems"][0], json!({ "type": "number" }));
        assert_eq!(v["prefixItems"][1], json!({ "type": "number" }));
    }

    #[test]
    fn record_map_has_properties_and_required() {
        // A Map with a String key "name" and an Int key "age".
        let s = Schema {
            serial_type: SerialType::Map,
            size: 2,
            width: 0,
            offsets: vec![],
            hint: None,
            parameters: vec![
                Schema::primitive(SerialType::String),
                Schema::primitive(SerialType::Sint32),
            ],
            keys: vec!["name".into(), "age".into()],
            name: None,
        };
        let v = schema_to_json_schema(&s);
        assert_eq!(v["type"], "object");
        assert_eq!(v["properties"]["name"], json!({ "type": "string" }));
        assert_eq!(v["properties"]["age"], json!({ "type": "integer" }));
        let req = v["required"].as_array().unwrap();
        assert!(req.contains(&Value::String("name".into())));
        assert!(req.contains(&Value::String("age".into())));
        assert_eq!(v["additionalProperties"], false);
    }

    #[test]
    fn optional_field_not_required_and_nullable() {
        // Record with one optional Int field "x".
        let s = Schema {
            serial_type: SerialType::Map,
            size: 1,
            width: 0,
            offsets: vec![],
            hint: None,
            parameters: vec![Schema {
                serial_type: SerialType::Optional,
                size: 1,
                width: 0,
                offsets: vec![],
                hint: None,
                parameters: vec![Schema::primitive(SerialType::Sint32)],
                keys: vec![],
                name: None,
            }],
            keys: vec!["x".into()],
            name: None,
        };
        let v = schema_to_json_schema(&s);
        assert_eq!(v["required"], json!([]));
        assert_eq!(v["properties"]["x"]["type"], json!(["integer", "null"]));
    }

    #[test]
    fn recur_does_not_infinite_loop() {
        let s = Schema {
            serial_type: SerialType::Recur,
            size: 0,
            width: 0,
            offsets: vec![],
            hint: None,
            parameters: vec![],
            keys: vec![],
            name: Some("Tree".into()),
        };
        assert_eq!(schema_to_json_schema(&s), json!({ "type": "object" }));
    }

    // -- MCP tool-shape (forward inputSchema + inverse slots) ----------------

    fn cmd_from_json(v: Value) -> Command {
        serde_json::from_value(v).expect("valid test command JSON")
    }

    #[test]
    fn defaulted_optional_becomes_typed_default_slot() {
        let cmd = cmd_from_json(json!({
            "name": "inc", "type": "pure",
            "return": { "schema": "j" },
            "args": [ { "kind": "opt", "schema": "j", "long": "count", "default": "5" } ],
        }));
        let shape = command_to_tool_shape(&cmd).expect("servable");
        // Forward: property present, optional (not required).
        assert!(shape.prop_names.contains("count"));
        assert!(shape.required.is_empty());
        // Inverse: one slot; omitted -> the typed default 5 (number, not "5").
        assert_eq!(shape.slots.len(), 1);
        // Optional is never in `required`; the slot supplies the typed default.
        assert!(shape.required.is_empty());
        match &shape.slots[0] {
            ArgSlot::Value { key, missing } => {
                assert_eq!(key, "count");
                assert_eq!(missing, &json!(5));
            }
            other => panic!("expected a Value slot, got {:?}", other),
        }
        // Scalar return -> no outputSchema.
        assert!(!shape.returns_object);
        assert!(shape.tool.get("outputSchema").is_none());
    }

    #[test]
    fn required_positional_is_required_indexed_key() {
        let cmd = cmd_from_json(json!({
            "name": "id", "type": "pure",
            "return": { "schema": "j" },
            // Two positionals: the metavar is ignored; keys are `_1`, `_2`.
            "args": [
                { "kind": "pos", "schema": "j", "metavar": "N" },
                { "kind": "pos", "schema": "j" }
            ],
        }));
        let shape = command_to_tool_shape(&cmd).expect("servable");
        assert_eq!(shape.required, vec!["_1".to_string(), "_2".to_string()]);
        match &shape.slots[0] {
            ArgSlot::Value { key, missing } => {
                assert_eq!(key, "_1");
                assert_eq!(missing, &Value::Null);
            }
            other => panic!("expected a Value slot, got {:?}", other),
        }
    }

    #[test]
    fn positionals_with_reused_metavar_no_longer_collide() {
        // Two positionals sharing a metavar used to collapse to one key and
        // exclude the command; indexed keys make them distinct + servable.
        let cmd = cmd_from_json(json!({
            "name": "cp", "type": "pure",
            "return": { "schema": "j" },
            "args": [
                { "kind": "pos", "schema": "s", "metavar": "FILE" },
                { "kind": "pos", "schema": "s", "metavar": "FILE" }
            ],
        }));
        let shape = command_to_tool_shape(&cmd).expect("servable (no collision)");
        assert!(shape.prop_names.contains("_1"));
        assert!(shape.prop_names.contains("_2"));
    }

    #[test]
    fn record_unrolled_flatten_inverse() {
        let cmd = cmd_from_json(json!({
            "name": "conf", "type": "pure",
            "return": { "schema": "j" },
            "args": [ {
                "kind": "grp", "schema": "m21aj1bs", "type": "Rec",
                "entries": [
                    { "key": "a", "arg": { "kind": "opt", "long": "a", "default": "3" } },
                    { "key": "b", "arg": { "kind": "opt", "long": "b", "default": "\"hi\"" } }
                ]
            } ],
        }));
        let shape = command_to_tool_shape(&cmd).expect("servable");
        // Forward: one property per field, keyed by field name.
        assert!(shape.prop_names.contains("a"));
        assert!(shape.prop_names.contains("b"));
        // Defaulted unrolled fields must NOT be advertised as required (the
        // inverse fills their defaults when omitted).
        assert!(
            shape.required.is_empty(),
            "defaulted unrolled fields must not be required, got {:?}",
            shape.required
        );
        // Inverse: a single Record slot assembled from the fields' defaults.
        assert_eq!(shape.slots.len(), 1);
        match &shape.slots[0] {
            ArgSlot::Record { group_key, fields } => {
                assert!(group_key.is_none());
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0].field, "a");
                assert_eq!(fields[0].default, json!(3));
                assert_eq!(fields[1].field, "b");
                assert_eq!(fields[1].default, json!("hi"));
            }
            other => panic!("expected a Record slot, got {:?}", other),
        }
    }

    #[test]
    fn record_return_gets_object_output_schema() {
        let cmd = cmd_from_json(json!({
            "name": "mk", "type": "pure",
            "return": { "schema": "m21aj1bs" },
            "args": [],
        }));
        let shape = command_to_tool_shape(&cmd).expect("servable");
        assert!(shape.returns_object);
        let out = shape.tool.get("outputSchema").expect("record return has outputSchema");
        assert_eq!(out["type"], "object");
    }

    #[test]
    fn stdin_positional_is_excluded() {
        let cmd = cmd_from_json(json!({
            "name": "cat", "type": "remote",
            "return": { "schema": "as" },
            "args": [ { "kind": "pos", "schema": "s", "metavar": "FILE", "stdin": true } ],
        }));
        assert!(command_to_tool_shape(&cmd).is_err());
    }

    #[test]
    fn stream_handle_return_is_excluded() {
        // 'F' is the IFile (stream-handle) wire schema.
        let cmd = cmd_from_json(json!({
            "name": "open", "type": "remote",
            "return": { "schema": "F" },
            "args": [],
        }));
        assert!(command_to_tool_shape(&cmd).is_err());
    }
}
