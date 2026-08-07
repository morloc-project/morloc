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
// --mcp-tools: MCP tools/list payload
// ---------------------------------------------------------------------------

fn build_mcp_tools(m: &Manifest) -> Value {
    let tools: Vec<Value> = m
        .commands
        .iter()
        .filter(|c| !c.internal)
        .map(command_to_mcp_tool)
        .collect();
    json!({ "tools": tools })
}

fn command_to_mcp_tool(cmd: &Command) -> Value {
    let mut props = Map::new();
    let mut required: Vec<Value> = Vec::new();
    let mut pos_index = 0usize;

    for arg in &cmd.args {
        match arg {
            Arg::Positional {
                schema,
                metavar,
                many,
                stdin,
                desc,
                ..
            } => {
                let name = metavar
                    .as_deref()
                    .map(|m| m.to_lowercase())
                    .unwrap_or_else(|| format!("arg{}", pos_index));
                let mut prop = mcp_type(schema.as_deref(), *many);
                set_description(&mut prop, desc, None);
                if !schema_is_optional(schema.as_deref()) && !*stdin {
                    required.push(Value::String(name.clone()));
                }
                props.insert(name, prop);
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
                props.insert(name, prop);
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
                props.insert(name, prop);
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
                if group_opt.is_some() {
                    // Whole record passed as one JSON value.
                    let name = type_desc
                        .as_deref()
                        .map(|t| t.to_lowercase())
                        .unwrap_or_else(|| "group".to_string());
                    let mut prop = parsed
                        .as_ref()
                        .map(schema_to_json_schema)
                        .unwrap_or_else(|| json!({ "type": "object" }));
                    set_description(&mut prop, desc, None);
                    props.insert(name, prop);
                } else if let Some(map) = parsed {
                    // Unrolled: one property per field, matching the flat flag
                    // surface. Field required unless its own schema is optional.
                    for entry in entries {
                        let field = map
                            .keys
                            .iter()
                            .position(|k| k == &entry.key)
                            .and_then(|i| map.parameters.get(i));
                        let prop = field
                            .map(schema_to_json_schema)
                            .unwrap_or_else(|| json!({}));
                        let is_req = field
                            .map(|f| f.serial_type != SerialType::Optional)
                            .unwrap_or(false);
                        if is_req {
                            required.push(Value::String(entry.key.clone()));
                        }
                        props.insert(entry.key.clone(), prop);
                    }
                }
            }
        }
    }

    let mut tool = Map::new();
    tool.insert("name".into(), Value::String(cmd.name.clone()));
    tool.insert(
        "description".into(),
        Value::String(cmd.desc.join("\n")),
    );
    tool.insert(
        "inputSchema".into(),
        json!({
            "type": "object",
            "properties": Value::Object(props),
            "required": required,
            "additionalProperties": false
        }),
    );
    // Output schema: omit for unit/nil returns.
    if let Some(parsed) = parse_schema(&cmd.ret.schema).ok() {
        if parsed.serial_type != SerialType::Nil {
            tool.insert("outputSchema".into(), schema_to_json_schema(&parsed));
        }
    }
    Value::Object(tool)
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
}
