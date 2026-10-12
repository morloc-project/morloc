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

/// Print a client `mcpServers` config entry (stdio transport) to stdout as
/// pure JSON. `nexus` is the absolute `morloc-nexus` path (the client's
/// `command`); `manifest` is the absolute manifest path passed to `mcp`. The
/// server is keyed by the program (module) name.
pub fn print_mcp_config(m: &Manifest, nexus: &str, manifest: &str) {
    let v = build_mcp_config(m, nexus, manifest);
    println!("{}", serde_json::to_string_pretty(&v).unwrap());
}

fn build_mcp_config(m: &Manifest, nexus: &str, manifest: &str) -> Value {
    let mut servers = serde_json::Map::new();
    servers.insert(
        m.name.clone(),
        json!({ "command": nexus, "args": ["mcp", manifest] }),
    );
    json!({ "mcpServers": Value::Object(servers) })
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
        // A closed constructor set becomes a JSON-Schema enum, so a
        // caller -- a person reading --json-help, or a model calling
        // the tool over MCP -- is handed the legal values instead of
        // a free string. This is what carrying the names in the wire
        // schema buys.
        Enum => json!({ "type": "string", "enum": s.keys }),
        // Externally tagged: either a bare constructor name (an arm with
        // no fields) or a one-key object whose key is the name.
        Variant => json!({
            "oneOf": [
                { "type": "string", "enum": s.keys },
                { "type": "object", "minProperties": 1, "maxProperties": 1,
                  "propertyNames": { "enum": s.keys } }
            ]
        }),
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
pub(crate) fn schema_is_optional(schema: Option<&str>) -> bool {
    morloc_manifest::call_args::schema_is_optional(schema)
}

/// A record field's wire schema in its record's schema.
fn group_field_schema<'a>(map: Option<&'a Schema>, entry: &morloc_manifest::GroupEntry) -> Option<&'a Schema> {
    map.and_then(|m| m.keys.iter().position(|k| k == &entry.key).and_then(|i| m.parameters.get(i)))
}

/// True when a record field must be given: it has no default and may not
/// be null. `field` is the field's wire schema.
fn group_field_required(entry: &morloc_manifest::GroupEntry, field: Option<&Schema>) -> bool {
    entry.arg.default_val().is_none()
        && !entry.arg.is_flag()
        && field.is_some_and(|f| f.serial_type != SerialType::Optional)
}

/// The help line a `data` argument needs and its type name alone does not
/// give: which constructors the argument accepts.
///
/// The names ride in the wire schema, which is how `--json-help` and the
/// MCP tool description have always been able to publish the closed set;
/// this puts the same set on the terminal, where a person types the value.
/// Optionals and arrays are looked through, since neither changes which
/// names are legal.
///
/// An argument-free constructor IS the value, so those are listed as
/// values. A payload-bearing one is a shape rather than a word, so those
/// are listed as constructors, each with the number of fields it takes.
pub(crate) fn schema_constructor_line(schema: Option<&str>) -> Option<String> {
    constructor_line_of(&parse_schema(schema?).ok()?)
}

/// The same line for one field of a record the CLI destructured into
/// options. A group entry carries no schema of its own -- dispatch
/// reassembles the record, so the group's schema is the one the manifest
/// records -- and the field's own schema is the parameter under its key.
pub(crate) fn group_entry_constructor_line(
    group_schema: Option<&str>,
    key: &str,
) -> Option<String> {
    let group = parse_schema(group_schema?).ok()?;
    let i = group.keys.iter().position(|k| k == key)?;
    constructor_line_of(group.parameters.get(i)?)
}

fn constructor_line_of(s: &Schema) -> Option<String> {
    match s.serial_type {
        SerialType::Enum => Some(format!("values: {}", s.keys.join(", "))),
        SerialType::Variant => Some(format!(
            "constructors: {}",
            s.keys
                .iter()
                .zip(s.parameters.iter())
                .map(|(k, arm)| if arm.size == 0 {
                    k.clone()
                } else {
                    format!("{k}/{}", arm.size)
                })
                .collect::<Vec<String>>()
                .join(", ")
        )),
        SerialType::Optional | SerialType::Array => {
            s.parameters.first().and_then(constructor_line_of)
        }
        SerialType::Nil | SerialType::Bool | SerialType::Sint8 | SerialType::Sint16 | SerialType::Sint32 | SerialType::Sint64 | SerialType::Uint8 | SerialType::Uint16 | SerialType::Uint32 | SerialType::Uint64 | SerialType::Float32 | SerialType::Float64 | SerialType::String | SerialType::Tuple | SerialType::Map | SerialType::Int | SerialType::Table | SerialType::Recur | SerialType::IFile | SerialType::OStream | SerialType::IStream => None,
    }
}

// ---------------------------------------------------------------------------
// --json-help: lossless morloc-native view
// ---------------------------------------------------------------------------

pub fn build_json_help(m: &Manifest) -> Value {
    let commands: Vec<Value> = m
        .commands
        .iter()
        .filter(|c| !c.internal)
        .map(|c| command_to_json(c, m))
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
            "epilogues": epilogues_json(&m.epilogues),
        },
        "groups": groups,
        "commands": commands,
        "types": named_type_glossary(m),
    })
}

/// Every named type the visible commands mention, defined once.
///
/// A rendered type says `[Hit]` or `RootedTree Str Real Str`; without the
/// definition a reader has the name and nothing else. Names are unique within a
/// program -- the compiler rejects two definitions of one name -- so the
/// glossary is flat and a name in any command's types resolves here.
///
/// Definitions are generic. A record lists its fields; a type whose wire form
/// comes from a Packable instance gives that form in its own parameters, so one
/// entry covers every instantiation rather than one entry per use.
fn named_type_glossary(m: &Manifest) -> Vec<Value> {
    let mut seen: Vec<&str> = Vec::new();
    let mut out: Vec<Value> = Vec::new();
    for cmd in m.commands.iter().filter(|c| !c.internal) {
        for t in &cmd.named_types {
            if seen.contains(&t.name.as_str()) {
                continue;
            }
            seen.push(&t.name);
            let mut entry = serde_json::Map::new();
            entry.insert("name".into(), json!(t.name));
            entry.insert("kind".into(), json!(t.kind));
            entry.insert("parameters".into(), json!(t.parameters));
            if t.kind == "packable" {
                entry.insert("equals".into(), json!(t.equals));
            } else if t.kind == "data" {
                entry.insert("desc".into(), json!(t.desc));
                let ctors: Vec<Value> = t
                    .constructors
                    .iter()
                    .map(|c| json!({ "name": c.name, "fields": c.fields, "desc": c.desc }))
                    .collect();
                entry.insert("constructors".into(), json!(ctors));
            } else {
                let fields: Vec<Value> = t
                    .fields
                    .iter()
                    .map(|f| json!({ "key": f.key, "type": f.type_desc }))
                    .collect();
                entry.insert("fields".into(), json!(fields));
            }
            out.push(Value::Object(entry));
        }
    }
    out
}

/// The `type` object for what a command puts on standard output: its
/// stream batch when it streams, its return value otherwise.
fn stream_or_return_type_object(cmd: &Command) -> Value {
    match &cmd.stream {
        Some(st) => type_object(
            non_empty(&st.schema),
            non_empty(&st.schema),
            non_empty(&st.type_desc),
        ),
        None => type_object(
            non_empty(&cmd.ret.schema),
            non_empty(&cmd.ret.general_schema),
            non_empty(&cmd.ret.type_desc),
        ),
    }
}

fn command_to_json(cmd: &Command, manifest: &Manifest) -> Value {
    let mut arguments: Vec<Value> = Vec::with_capacity(cmd.args.len());
    let mut pos_index = 0usize;
    for arg in &cmd.args {
        arguments.push(arg_to_json(arg, pos_index));
        if matches!(arg, Arg::Positional { .. }) {
            pos_index += 1;
        }
    }

    // Each action flag selects a different output type, so the type
    // belongs on the terminal rather than only on the command's own
    // return. Resolved through the synthesized entry the flag dispatches
    // to; a `render` terminal writes those bytes verbatim, which the
    // sibling `render` flag already marks.
    let terminals: Vec<Value> = cmd
        .terminals
        .iter()
        .map(|t| {
            let entry = t.output_command(manifest);
            let ty = entry.map(|c| stream_or_return_type_object(c));
            json!({
                "short": t.short.map(|c| c.to_string()),
                "long": t.long,
                "description": t.description,
                "render": t.render,
                "default": t.default,
                "type": ty,
            })
        })
        .collect();

    let ret_schema = non_empty(&cmd.ret.schema);
    let ret_general = non_empty(&cmd.ret.general_schema);
    let ret_type = non_empty(&cmd.ret.type_desc);

    json!({
        "name": cmd.name,
        "kind": if cmd.is_pure() { "pure" } else { "remote" },
        "group": cmd.group,
        "description": cmd.desc,
        "arguments": arguments,
        "return": {
            "description": cmd.ret.desc,
            // True when the command writes a stream to standard output
            // instead of returning a value. Its morloc return type is
            // `()`; `type` below describes what actually reaches stdout.
            "streaming": cmd.stream.is_some(),
            "type": match &cmd.stream {
                Some(st) => type_object(
                    non_empty(&st.schema),
                    non_empty(&st.schema),
                    non_empty(&st.type_desc),
                ),
                None => type_object(ret_schema, ret_general, ret_type),
            },
        },
        "terminals": terminals,
        "epilogues": epilogues_json(&cmd.epilogues),
    })
}

/// One verbatim string per `@epilogue` block, its lines joined by newlines.
fn epilogues_json(epilogues: &[Vec<String>]) -> Vec<String> {
    epilogues.iter().map(|block| block.join("\n")).collect()
}

/// Render one manifest [`Arg`] to its lossless JSON object. `pos_index` is the
/// zero-based position among positional args (ignored for non-positionals).
fn arg_to_json(arg: &Arg, pos_index: usize) -> Value {
    let mut v = arg_fields_json(arg, pos_index);
    // Present only on an argument with `@parse` formats.
    if let (Some(p), Value::Object(obj)) = (arg.parse(), &mut v) {
        obj.insert("parse".into(), parse_json(p));
    }
    v
}

fn arg_fields_json(arg: &Arg, pos_index: usize) -> Value {
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
            json!({
                "name": arg.key(),
                "role": "positional",
                "position": pos_index,
                "metavar": metavar,
                "required": !schema_is_optional(schema.as_deref()) && !*stdin,
                "variadic": many,
                "stdin": stdin,
                "quoted": quoted,
                "default": Value::Null,
                "description": desc,
                "type": type_object(
                    schema.as_deref(),
                    arg.general_schema_str(),
                    type_desc.as_deref(),
                ),
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
            json!({
                "name": arg.key(),
                "role": "option",
                "metavar": metavar,
                "required": false,
                "variadic": many,
                "quoted": quoted,
                "short": short_opt,
                "long": long_opt,
                "default": default_val,
                "description": desc,
                "type": type_object(
                    schema.as_deref(),
                    arg.general_schema_str(),
                    type_desc.as_deref(),
                ),
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
            short_rev,
            default_val,
            desc,
            ..
        } => {
            json!({
                "name": arg.key(),
                "role": "flag",
                "required": false,
                "short": short_opt,
                "long": long_opt,
                "long_reverse": long_rev,
                "short_reverse": short_rev,
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
            let name = arg.key().to_string();
            let entries_json: Vec<Value> = entries
                .iter()
                .map(|e| json!({ "key": e.key, "argument": arg_to_json(&e.arg, 0) }))
                .collect();
            let group_option = group_opt.as_ref().map(|g| {
                json!({ "short": g.short_opt, "long": g.long_opt })
            });
            let map = schema.as_deref().and_then(|s| parse_schema(s).ok());
            let required = entries.iter().any(|e| group_field_required(e, group_field_schema(map.as_ref(), e)));
            json!({
                "name": name,
                "role": "group",
                "metavar": metavar,
                "required": required,
                "description": desc,
                "type": type_object(
                    schema.as_deref(),
                    arg.general_schema_str(),
                    type_desc.as_deref(),
                ),
                "named_type_kind": arg.kind_constraint(),
                "group_option": group_option,
                "entries": entries_json,
            })
        }
        Arg::Alt {
            schema,
            type_desc,
            metavar,
            desc,
            required,
            default_val,
            arms,
            ..
        } => {
            let arms_json: Vec<Value> = arms
                .iter()
                .map(|a| {
                    let fields: Vec<Value> = a
                        .fields
                        .iter()
                        .map(|f| json!({ "type": f.type_desc, "wire": f.schema }))
                        .collect();
                    json!({
                        "constructor": a.ctor,
                        "long": a.long,
                        "description": a.desc,
                        "fields": fields,
                    })
                })
                .collect();
            json!({
                "name": arg.key(),
                "role": "alternatives",
                "metavar": metavar,
                "required": required,
                "default": default_val,
                "description": desc,
                "type": type_object(
                    schema.as_deref(),
                    arg.general_schema_str(),
                    type_desc.as_deref(),
                ),
                "arms": arms_json,
            })
        }
    }
}

/// Build the `type` object: morloc type string, compact wire schema, and the
/// structured JSON Schema. Any component with no data is omitted.
///
/// `wire` publishes the *general* schema, not the one dispatch uses. The
/// concrete schema carries a hint naming the container the pool's language
/// builds (`<dict>` for Python, `<Point>` with no binding declared), so
/// publishing it would make the advertised data contract change when the
/// implementation language changed and nothing observable did.
fn type_object(
    schema: Option<&str>,
    general_schema: Option<&str>,
    type_desc: Option<&str>,
) -> Value {
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
    if let Some(sc) = general_schema.or(schema) {
        obj.insert("wire".into(), Value::String(sc.to_string()));
    }
    if let Some(p) = parsed.as_ref() {
        obj.insert("structure".into(), schema_to_json_schema(p));
    }
    Value::Object(obj)
}

/// An argument's `@parse` formats as JSON: each format's name and the
/// extensions that select it. The CLI reads formats; daemon, MCP and
/// call-packet callers pass values.
fn parse_json(p: &morloc_manifest::ArgParse) -> Value {
    json!({
        "formats": p.formats.iter()
            .map(|f| json!({ "name": f.name, "exts": f.exts }))
            .collect::<Vec<_>>(),
    })
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
    morloc_manifest::cli_value::source_atom_name(*s)
}

fn form_str(f: &FormAtom) -> &'static str {
    morloc_manifest::cli_value::form_atom_name(*f)
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

fn non_empty(s: &str) -> Option<&str> {
    if s.is_empty() {
        None
    } else {
        Some(s)
    }
}

// ---------------------------------------------------------------------------
// --mcp-tools: MCP tools/list payload
// ---------------------------------------------------------------------------
//
// A tool's properties are the names the runtime's argument resolver
// (morloc_manifest::call_args) accepts, so a `tools/call` forwards its
// arguments as they arrived.

/// The MCP shape of one command: the `tools/list` entry and the output
/// actions `_render` may select.
pub struct McpToolShape {
    pub name: String,
    /// The MCP tool object for `tools/list`.
    pub tool: Value,
    /// The values `_render` may take besides `raw`: every output action's long
    /// name (empty when the command has none).
    pub actions: Vec<String>,
}

/// True when a return's parsed wire schema is a Map/record -- the single
/// condition under which MCP emits `structuredContent` + an object
/// `outputSchema`.
fn schema_returns_object(p: &Schema) -> bool {
    p.serial_type == SerialType::Map
}

/// True when a wire schema tree contains a type the MCP tool surface cannot
/// safely serve: a stream handle (`IFile`/`IStream`/`OStream`, which would
/// need per-call stdout capture). Checked on every argument and the return
/// type.
fn schema_tree_excluded(s: &Schema) -> bool {
    use SerialType::*;
    // A stream handle or an IFile cannot be served: the handle means nothing
    // to a client, and a streaming command writes to a stdout fd the MCP loop
    // has aliased away. A Table is neither -- its row schema is known
    // statically and `schema_to_json_schema` already renders it as an array
    // of typed row objects, which is a shape a client understands.
    matches!(s.serial_type, IFile | IStream | OStream)
        || s.parameters.iter().any(schema_tree_excluded)
}

/// A schema that does not parse cannot be marshaled either, so its command
/// is excluded.
fn schema_str_excluded(schema: Option<&str>) -> bool {
    schema.is_some_and(|s| parse_schema(s).map_or(true, |p| schema_tree_excluded(&p)))
}

fn build_mcp_tools(m: &Manifest) -> Value {
    let tools: Vec<Value> = build_tool_shapes(m).into_iter().map(|s| s.tool).collect();
    json!({ "tools": tools })
}

/// Non-internal commands servable over the JSON/packet wire, in manifest
/// order. Applies the same exclusions as [`build_tool_shapes`] (a stream-handle
/// return, unparseable schemas, property-name collisions) but emits
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
/// cannot serve (a stream-handle return, unparseable schemas, or a
/// property-name collision) are dropped with a note on stderr.
pub fn build_tool_shapes(m: &Manifest) -> Vec<McpToolShape> {
    m.commands
        .iter()
        .filter(|c| !c.internal)
        .filter_map(|c| match command_to_tool_shape(c) {
            Ok(mut shape) => {
                describe_actions(&mut shape, c, m);
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
    use morloc_manifest::call_args::required_off_cli;
    // A file-handle return means nothing to a client. A file-path argument is
    // served: it names a file on the server (IFACE-3).
    for arg in &cmd.args {
        if arg.schema_str().is_some_and(|s| parse_schema(s).is_err()) {
            return Err("argument has a schema that does not parse".into());
        }
    }
    if schema_str_excluded(non_empty(&cmd.ret.schema)) {
        return Err("return has a stream-handle type or a schema that does not parse".into());
    }

    let mut props = Map::new();
    let mut required: Vec<Value> = Vec::new();
    let unambiguous = morloc_manifest::call_args::unambiguous_names(cmd);

    // Insert a property, failing closed on a name collision (two arguments
    // under one name could not both be given).
    let insert_prop = |props: &mut Map<String, Value>, name: &str, prop: Value| -> Result<(), String> {
        if props.contains_key(name) {
            return Err(format!("property name collision on '{}'", name));
        }
        props.insert(name.to_string(), prop);
        Ok(())
    };

    for arg in &cmd.args {
        let name = arg.key().to_string();
        if required_off_cli(arg) {
            required.push(Value::String(name.clone()));
        }
        match arg {
            Arg::Positional { schema, many, desc, .. } => {
                let mut prop = mcp_type(schema.as_deref(), *many);
                set_description(&mut prop, &with_ctor_notes(cmd, arg.type_desc_str(), desc), None);
                insert_prop(&mut props, &name, prop)?;
            }
            Arg::Optional { schema, many, default_val, desc, .. } => {
                let mut prop = mcp_type(schema.as_deref(), *many);
                set_description(&mut prop, &with_ctor_notes(cmd, arg.type_desc_str(), desc), default_val.as_deref());
                insert_prop(&mut props, &name, prop)?;
            }
            Arg::Flag { default_val, desc, .. } => {
                let mut prop = json!({ "type": "boolean" });
                set_description(&mut prop, desc, default_val.as_deref());
                insert_prop(&mut props, &name, prop)?;
            }
            Arg::Group { schema, group_opt, entries, desc, .. } => {
                let parsed = schema.as_deref().and_then(|s| parse_schema(s).ok());
                // The whole record under the group's name, when the command
                // line takes it whole; each field under its own name always.
                // A field is required only when nothing else can fill it.
                if group_opt.is_some() {
                    let mut prop = parsed
                        .as_ref()
                        .map(schema_to_json_schema)
                        .unwrap_or_else(|| json!({ "type": "object" }));
                    set_description(&mut prop, desc, None);
                    insert_prop(&mut props, &name, prop)?;
                }
                for entry in entries {
                    // A field whose name another argument shares is given
                    // through the whole record.
                    if group_opt.is_some() && !unambiguous.contains(&entry.key) {
                        continue;
                    }
                    let field = group_field_schema(parsed.as_ref(), entry);
                    let mut prop = if entry.arg.is_flag() {
                        json!({ "type": "boolean" })
                    } else {
                        field.map(schema_to_json_schema).unwrap_or_else(|| json!({}))
                    };
                    set_description(&mut prop, entry.arg.desc_lines(), entry.arg.default_val());
                    if group_opt.is_none() && group_field_required(entry, field) {
                        required.push(Value::String(entry.key.clone()));
                    }
                    insert_prop(&mut props, &entry.key, prop)?;
                }
            }
            // An unrolled `data` is one property holding the whole value: a
            // model writes the constructor JSON directly, the arms being a
            // command-line convenience.
            Arg::Alt { schema, default_val, desc, .. } => {
                let mut prop = mcp_type(schema.as_deref(), false);
                set_description(&mut prop, &with_ctor_notes(cmd, arg.type_desc_str(), desc), default_val.as_deref());
                insert_prop(&mut props, &name, prop)?;
            }
        }
    }

    // Output-projection selection. A command with `@render`/`@with` terminals
    // exposes each projection through a synthetic `_render` enum on the parent
    // tool; `"raw"` (the default) returns the command's own typed value. The
    // per-target media type is filled from the manifest in `build_tool_shapes`.
    // `_render` is a routing property, not a pool argument, and is not
    // forwarded with the arguments. The leading `_` uses the same reserved namespace as positional keys (`_1`, ...), which
    // an argument flag can never produce -- so it cannot collide with a real arg.
    let actions: Vec<String> = cmd.terminals.iter().map(|t| t.long.clone()).collect();
    if !actions.is_empty() {
        let enum_vals: Vec<Value> = std::iter::once("raw".to_string())
            .chain(actions.iter().cloned())
            .map(Value::String)
            .collect();
        // A call that selects nothing gets the CLI's output.
        let default = cmd
            .terminals
            .iter()
            .find(|t| t.default)
            .map(|t| t.long.clone())
            .unwrap_or_else(|| "raw".to_string());
        let prop = json!({
            "type": "string",
            "enum": enum_vals,
            "default": default,
            "description":
                "Output action: 'raw' returns the command's own value; any other \
                 value runs that output action, as its command-line flag does."
        });
        insert_prop(&mut props, "_render", prop)?;
    }

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
    // `outputSchema` would be invalid; those results ride in a text block. A
    // command with output actions declares none: an action's result need not
    // match it.
    if returns_object && actions.is_empty() {
        if let Some(ref parsed) = ret_parsed {
            tool.insert("outputSchema".into(), schema_to_json_schema(parsed));
        }
    }

    Ok(McpToolShape {
        name: cmd.name.clone(),
        tool: Value::Object(tool),
        actions,
    })
}

/// Append the command's output actions to its tool description, one row per
/// action as `-h` prints them (CLI-11): the action, its output's media type or
/// type, `(raw bytes)` for a `@render` action, and `default`. Split from
/// `command_to_tool_shape`, which sees one command, because an action's output
/// type is its entry command's return.
fn describe_actions(shape: &mut McpToolShape, cmd: &Command, m: &Manifest) {
    if cmd.terminals.is_empty() {
        return;
    }
    let streams = cmd.stream.is_some() || cmd.terminals.iter().any(|t| t.kind.of_streaming_command());
    let raw = if streams {
        "streams its output; runs only from the command line".to_string()
    } else {
        cmd.ret.type_desc.clone()
    };
    let mut rows = vec!["Output actions (`_render`):".to_string(), format!("  raw: {}", raw)];
    for t in &cmd.terminals {
        let entry = t.entry.as_deref().and_then(|e| m.command_by_name(e));
        let out = match entry {
            _ if t.kind == morloc_manifest::ActionKind::Stream => {
                "streams its output; runs only from the command line".to_string()
            }
            Some(e) => e.ret.mime.clone().unwrap_or_else(|| e.ret.type_desc.clone()),
            None => "runs only from the command line".to_string(),
        };
        let mut row = format!("  {}: {}", t.long, out);
        if t.render {
            row.push_str(" (raw bytes)");
        }
        if t.default {
            row.push_str(", default");
        }
        if !t.description.is_empty() {
            row.push_str(&format!(" -- {}", t.description));
        }
        rows.push(row);
    }
    if let Some(Value::String(d)) = shape.tool.get_mut("description") {
        if !d.is_empty() {
            d.push_str("\n\n");
        }
        d.push_str(&rows.join("\n"));
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
/// An argument's description lines, followed by one line per described
/// constructor when the argument's type is a `data` in the command's
/// glossary. A model reading the tool sees what each name means, not just
/// the closed set.
fn with_ctor_notes(
    cmd: &Command,
    type_desc: Option<&str>,
    desc: &[std::string::String],
) -> Vec<std::string::String> {
    let mut out: Vec<std::string::String> = desc.to_vec();
    let Some(tn) = type_desc else { return out };
    // The type may be wrapped (`?Color`, `[Color]`); the glossary name is
    // the bare one.
    let bare = tn.trim_start_matches(['?', '[']).trim_end_matches(']');
    if let Some(t) = cmd
        .named_types
        .iter()
        .find(|t| t.kind == "data" && t.name == bare)
    {
        let notes: Vec<std::string::String> = t
            .constructors
            .iter()
            .filter_map(|c| {
                let note: Vec<&str> =
                    c.desc.iter().map(|l| l.as_str()).filter(|l| !l.is_empty()).collect();
                if note.is_empty() {
                    None
                } else {
                    Some(format!("{}: {}", c.name, note.join(" ")))
                }
            })
            .collect();
        if !notes.is_empty() {
            out.push(notes.join("; "));
        }
    }
    out
}

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

    // -- MCP tool shape ------------------------------------------------------

    fn cmd_from_json(v: Value) -> Command {
        serde_json::from_value(v).expect("valid test command JSON")
    }

    fn props(shape: &McpToolShape) -> Vec<String> {
        shape.tool["inputSchema"]["properties"].as_object().unwrap().keys().cloned().collect()
    }

    fn required(shape: &McpToolShape) -> Vec<String> {
        shape.tool["inputSchema"]["required"]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_str().unwrap().to_string())
            .collect()
    }

    #[test]
    fn a_defaulted_option_is_an_optional_property() {
        let cmd = cmd_from_json(json!({
            "name": "inc", "type": "pure",
            "return": { "schema": "j" },
            "args": [ { "kind": "opt", "key": "count", "schema": "j", "long": "count", "default": "5" } ],
        }));
        let shape = command_to_tool_shape(&cmd).expect("servable");
        assert_eq!(props(&shape), vec!["count"]);
        assert!(required(&shape).is_empty());
        assert!(shape.tool.get("outputSchema").is_none());
    }

    #[test]
    fn positionals_are_keyed_by_name_or_index_and_required() {
        let cmd = cmd_from_json(json!({
            "name": "id", "type": "pure",
            "return": { "schema": "j" },
            // Two positionals sharing a metavar: keys come from the manifest.
            "args": [
                { "kind": "pos", "key": "records", "schema": "j", "metavar": "FILE" },
                { "kind": "pos", "key": "_2", "schema": "j", "metavar": "FILE" },
                { "kind": "pos", "key": "_3", "schema": "?j" }
            ],
        }));
        let shape = command_to_tool_shape(&cmd).expect("servable");
        assert_eq!(props(&shape), vec!["records", "_2", "_3"]);
        assert_eq!(required(&shape), vec!["records", "_2"]);
    }

    #[test]
    fn file_path_and_stdin_arguments_are_served_and_stdin_is_required() {
        let cmd = cmd_from_json(json!({
            "name": "cat", "type": "remote",
            "return": { "schema": "s" },
            "args": [
                { "kind": "pos", "key": "_1", "schema": "?s", "stdin": true },
                { "kind": "pos", "key": "_2", "schema": "F" }
            ],
        }));
        let shape = command_to_tool_shape(&cmd).expect("servable");
        assert_eq!(required(&shape), vec!["_1", "_2"]);
        let handle_return = cmd_from_json(json!({
            "name": "open", "type": "remote", "return": { "schema": "F" }, "args": [],
        }));
        assert!(command_to_tool_shape(&handle_return).is_err());
    }

    #[test]
    fn a_record_is_named_by_its_fields_and_whole_when_the_cli_takes_it_whole() {
        let group = |group_opt: Value| cmd_from_json(json!({
            "name": "conf", "type": "pure",
            "return": { "schema": "j" },
            "args": [ {
                "kind": "grp", "key": "rec", "schema": "m21aj1bs", "type": "Rec",
                "group_opt": group_opt,
                "entries": [
                    { "key": "a", "arg": { "kind": "opt", "key": "a", "long": "a", "default": "3" } },
                    { "key": "b", "arg": { "kind": "opt", "key": "b", "long": "b" } }
                ]
            } ],
        }));
        let unrolled = command_to_tool_shape(&group(Value::Null)).expect("servable");
        assert_eq!(props(&unrolled), vec!["a", "b"]);
        assert_eq!(required(&unrolled), vec!["b"]);
        let whole = command_to_tool_shape(&group(json!({ "long": "rec" }))).expect("servable");
        assert_eq!(props(&whole), vec!["rec", "a", "b"]);
        assert!(required(&whole).is_empty());
    }

    #[test]
    fn a_record_field_sharing_a_name_is_given_through_the_whole_record() {
        let cmd = cmd_from_json(json!({
            "name": "conf", "type": "pure",
            "return": { "schema": "j" },
            "args": [
                { "kind": "opt", "key": "a", "schema": "j", "long": "a", "default": "1" },
                { "kind": "grp", "key": "rec", "schema": "m21aj1bs", "group_opt": { "long": "rec" },
                  "entries": [
                    { "key": "a", "arg": { "kind": "opt", "key": "a", "long": "ra", "default": "3" } },
                    { "key": "b", "arg": { "kind": "opt", "key": "b", "long": "b", "default": "\"x\"" } }
                  ] }
            ],
        }));
        let shape = command_to_tool_shape(&cmd).expect("servable");
        assert_eq!(props(&shape), vec!["a", "rec", "b"]);
    }

    #[test]
    fn record_return_gets_object_output_schema() {
        let cmd = cmd_from_json(json!({
            "name": "mk", "type": "pure",
            "return": { "schema": "m21aj1bs" },
            "args": [],
        }));
        let shape = command_to_tool_shape(&cmd).expect("servable");
        assert!(shape.tool.get("outputSchema").is_some());
        let out = shape.tool.get("outputSchema").expect("record return has outputSchema");
        assert_eq!(out["type"], "object");
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
