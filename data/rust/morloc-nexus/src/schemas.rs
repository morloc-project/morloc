//! Schema-walking renderer for the Record / Table Schemas blocks
//! shown under per-command `--help` output.
//!
//! The named-type layout for record / object / table args is
//! reconstructed by walking the command's `args` + `return` value:
//! for every typed entry whose schema parses to a `Map` at the top
//! level, the entry's `type` name labels it and its schema's keys +
//! parameter schemas give the field list. The record-vs-table
//! distinction comes from the entry's `kind` constraint.
//!
//! The renderer is parser-agnostic -- it walks the manifest, not
//! argv -- and the rendered block is plumbed into clap's
//! `after_help` slot by [`crate::phase2::build_root`] so each
//! per-command `--help` shows the named-type field layouts it
//! references.
//!
//! The public surface is [`render_command_schemas`]: given a
//! manifest [`Command`], it returns the rendered block as an owned
//! `String` (or `None` when no named-type args appear in the
//! signature).

use morloc_manifest::{Arg, Command};

/// A rendered named-type layout, sourced from a parsed schema.
struct TypeLayout<'a> {
    name: &'a str,
    /// "record" | "object" | "table" -- from the `kind` constraint.
    kind: &'a str,
    /// (field_name, rendered_type) pairs.
    fields: Vec<(String, String)>,
}

/// Pretty-render a parsed `Schema` as a morloc-flavored type string,
/// suitable for the field-type column in the schemas block.
fn render_schema_type(s: &morloc_runtime_types::schema::Schema) -> String {
    use morloc_runtime_types::schema::SerialType::*;
    match s.serial_type {
        Nil => "()".into(),
        Bool => "Bool".into(),
        Sint8 => "I8".into(),
        Sint16 => "I16".into(),
        Sint32 => "Int".into(),
        Sint64 => "I64".into(),
        Uint8 => "U8".into(),
        Uint16 => "U16".into(),
        Uint32 => "U32".into(),
        Uint64 => "U64".into(),
        Float32 => "F32".into(),
        Float64 => "F64".into(),
        String => "Str".into(),
        Array => format!(
            "[{}]",
            s.parameters
                .first()
                .map(render_schema_type)
                .unwrap_or_else(|| "?".into())
        ),
        Tuple => {
            // `String` here is fully qualified because the surrounding
            // match brings `SerialType::String` into scope as a variant,
            // shadowing the std `String` type.
            let inner: Vec<std::string::String> =
                s.parameters.iter().map(render_schema_type).collect();
            format!("({})", inner.join(", "))
        }
        Map => {
            // A nested record-ish thing. Use the hint string when
            // present (which carries the language-specific concrete
            // type name); otherwise show an inline placeholder. The
            // nested record will be listed separately in the same
            // schema block if its name appears as another arg's type.
            s.hint.clone().unwrap_or_else(|| "{..}".into())
        }
        Optional => format!(
            "?{}",
            s.parameters
                .first()
                .map(render_schema_type)
                .unwrap_or_else(|| "?".into())
        ),
        Int => "Int".into(),
        Table => {
            // Table primitive: bare `T` renders as `Table` (any
            // schema); `T:K<entries>` renders as `Table {k1=t1, ...}`
            // so help text shows the declared columns the same way
            // the user wrote them.
            if s.parameters.is_empty() {
                "Table".into()
            } else {
                let cols: Vec<std::string::String> = s
                    .parameters
                    .iter()
                    .enumerate()
                    .map(|(i, p)| {
                        let key = s.keys.get(i).cloned().unwrap_or_default();
                        format!("{}={}", key, render_schema_type(p))
                    })
                    .collect();
                format!("Table {{{}}}", cols.join(", "))
            }
        }
        // Recursive back-reference. Render as the declared name so
        // help text shows e.g. `Tree` where the body would otherwise
        // recurse.
        Recur => s.name.clone().unwrap_or_else(|| "?".into()),
        // Cross-pool stream handles: surface as their user-facing morloc
        // type. The wire is a tagged union of path / handle; help text
        // shows only the type layer.
        IFile => "IFile a".into(),
        OStream => "OStream a".into(),
        IStream => "IStream a".into(),
    }
}

/// Try to extract a `TypeLayout` from a (name, schema_string, kind)
/// triple. Returns None if any input is missing or the schema does
/// not parse to a top-level Map. Tables (whose fields are arrays in
/// the wire schema) render their fields by the array's element type,
/// mirroring how the user wrote them in the source.
fn extract_named_layout<'a>(
    type_name: Option<&'a str>,
    schema_str: Option<&str>,
    kind: Option<&'a str>,
) -> Option<TypeLayout<'a>> {
    use morloc_runtime_types::schema::SerialType;
    let name = type_name?;
    let schema = schema_str?;
    let kind = kind?;
    let parsed = morloc_runtime_types::schema::parse_schema(schema).ok()?;
    if parsed.serial_type != SerialType::Map {
        return None;
    }
    // For a table, every field's wire schema is an Array -- peel one
    // layer so the user sees `name :: Str` not `name :: [Str]`.
    let strip_array = kind == "table";
    let fields = parsed
        .keys
        .iter()
        .zip(parsed.parameters.iter())
        .map(|(k, p)| {
            let inner = if strip_array && p.serial_type == SerialType::Array {
                p.parameters.first().unwrap_or(p)
            } else {
                p
            };
            (k.clone(), render_schema_type(inner))
        })
        .collect();
    Some(TypeLayout { name, kind, fields })
}

/// Walk every arg + the return of a command. For each typed entry,
/// build a layout. Deduplicate by type name, preserving discovery
/// order so the rendering matches the order types appear in the
/// signature.
fn collect_command_layouts<'a>(cmd: &'a Command) -> Vec<TypeLayout<'a>> {
    use std::collections::HashSet;
    let mut seen: HashSet<&str> = HashSet::new();
    let mut out: Vec<TypeLayout<'a>> = Vec::new();

    for arg in &cmd.args {
        // Skip unrolled groups without a group_opt: each field
        // already appears as its own flag in the usage, so the schema
        // is redundant. Keep the schema when group_opt is present
        // (the user can pass the entire record as JSON and needs the
        // full field spec).
        if let Arg::Group { group_opt: None, .. } = arg {
            continue;
        }
        if let Some(layout) =
            extract_named_layout(arg.type_desc_str(), arg.schema_str(), arg.kind_constraint())
        {
            if seen.insert(layout.name) {
                out.push(layout);
            }
        }
    }

    let ret_kind = cmd
        .ret
        .constraints
        .iter()
        .find(|c| c.ctype == "kind")
        .and_then(|c| c.value.as_ref().and_then(|v| v.as_str()));
    if let Some(layout) =
        extract_named_layout(Some(&cmd.ret.type_desc), Some(&cmd.ret.schema), ret_kind)
    {
        if seen.insert(layout.name) {
            out.push(layout);
        }
    }

    out
}

/// Render a list of layouts to a String. Each layout shows its type
/// name on its own line followed by the field list with `::`-aligned
/// column widths. Definitions are separated by blank lines.
fn render_layouts(defs: &[&TypeLayout]) -> String {
    let mut out = String::new();
    for (i, def) in defs.iter().enumerate() {
        if i > 0 {
            out.push('\n');
        }
        out.push_str(&format!("  {}\n", def.name));

        let name_width = def
            .fields
            .iter()
            .map(|(k, _)| k.len())
            .max()
            .unwrap_or(0);
        for (k, v) in &def.fields {
            out.push_str(&format!("    {:width$} :: {}\n", k, v, width = name_width));
        }
    }
    out
}

/// Render the Record Schemas / Table Schemas sections for any named
/// types referenced in this command's signature. Returns `None` when
/// there are no named types (so callers can suppress the whole
/// "after help" block).
pub fn render_command_schemas(cmd: &Command) -> Option<String> {
    let layouts = collect_command_layouts(cmd);
    if layouts.is_empty() {
        return None;
    }
    let records: Vec<&TypeLayout> = layouts.iter().filter(|l| l.kind != "table").collect();
    let tables: Vec<&TypeLayout> = layouts.iter().filter(|l| l.kind == "table").collect();

    let mut out = String::new();
    if !records.is_empty() {
        out.push_str("Record Schemas:\n");
        out.push_str(&render_layouts(&records));
    }
    if !tables.is_empty() {
        if !out.is_empty() {
            out.push('\n');
        }
        out.push_str("Table Schemas:\n");
        out.push_str(&render_layouts(&tables));
    }
    // Drop the trailing newline so clap can append its own block
    // separator cleanly.
    if out.ends_with('\n') {
        out.pop();
    }
    Some(out)
}
