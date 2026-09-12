//! Renderer for the Record / Table Schemas blocks shown under
//! per-command `--help` output.
//!
//! The layouts come from the command's `named_types`, which the
//! compiler fills by walking the whole signature: a wire schema cannot
//! carry a morloc type name, so a renderer keyed on wire schemas alone
//! loses a name as soon as its record is wrapped in a list (`[Hit]` is
//! not a name to key on). Only the compiler, which has the type, can
//! say what the names are.
//!
//! The renderer is parser-agnostic -- it walks the manifest, not
//! argv -- and the rendered block is plumbed into clap's
//! `after_help` slot by [`crate::phase2::build_root`] so each
//! per-command `--help` shows the named-type field layouts it
//! references.
//!
//! The public surface is [`render_command_schemas`]: given a
//! manifest [`Command`], it returns the rendered block as an owned
//! `String` (or `None` when the signature names no such type).

use morloc_manifest::{Command, NamedType};

/// Pretty-render a parsed `Schema` as a morloc-flavored type string,
/// suitable for the field-type column in the schemas block.
///
/// `self_ref`, when set, is a `(short_name, full_name)` pair for the
/// record currently being rendered. Any `Recur` back-reference whose
/// short name matches `short_name` is expanded to `full_name` so the
/// parameterization of the outer type (`Container Int`) surfaces on
/// self-referential fields, rather than collapsing to the bare
/// constructor name (`Container`). Parens are added when the
/// parameterized name contains a space so the surrounding context
/// (e.g. an `?` wrapper) binds correctly.
pub(crate) fn render_schema_type(
    s: &morloc_runtime_types::schema::Schema,
    self_ref: Option<(&str, &str)>,
) -> String {
    use morloc_runtime_types::schema::SerialType::*;
    match s.serial_type {
        Nil => "()".into(),
        Bool => "Bool".into(),
        // Show the constructor set: it is the useful thing to know
        // about the type and it always fits, being closed.
        Enum => s.keys.join(" | "),
        // Show each arm with its field count, which is what distinguishes
        // the arms from an enum's bare names.
        Variant => s
            .keys
            .iter()
            .zip(s.parameters.iter())
            .map(|(k, arm)| if arm.size == 0 { k.clone() } else { format!("{k}/{}", arm.size) })
            .collect::<Vec<_>>()
            .join(" | "),
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
                .map(|p| render_schema_type(p, self_ref))
                .unwrap_or_else(|| "?".into())
        ),
        Tuple => {
            // `String` here is fully qualified because the surrounding
            // match brings `SerialType::String` into scope as a variant,
            // shadowing the std `String` type.
            let inner: Vec<std::string::String> = s
                .parameters
                .iter()
                .map(|p| render_schema_type(p, self_ref))
                .collect();
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
        Optional => {
            let inner = s
                .parameters
                .first()
                .map(|p| render_schema_type(p, self_ref))
                .unwrap_or_else(|| "?".into());
            if inner.contains(' ') {
                format!("?({})", inner)
            } else {
                format!("?{}", inner)
            }
        }
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
                        format!("{}={}", key, render_schema_type(p, self_ref))
                    })
                    .collect();
                format!("Table {{{}}}", cols.join(", "))
            }
        }
        // Recursive back-reference. Wire schemas carry only the bare
        // constructor name here (parameter applications are not
        // encoded), so substitute the outer record's full parameterized
        // display name when the reference points back at it.
        Recur => {
            let raw = s.name.clone().unwrap_or_else(|| "?".into());
            match self_ref {
                Some((short, full)) if raw == short => full.into(),
                _ => raw,
            }
        }
        // Cross-pool stream handles: surface as their user-facing morloc
        // type. The wire is a tagged union of path / handle; help text
        // shows only the type layer.
        IFile => "IFile a".into(),
        OStream => "OStream a".into(),
        IStream => "IStream a".into(),
    }
}

/// Render the Record Schemas / Table Schemas sections for any named
/// types referenced in this command's signature. Returns `None` when
/// there are no named types (so callers can suppress the whole
/// "after help" block).
pub fn render_command_schemas(cmd: &Command) -> Option<String> {
    if cmd.named_types.is_empty() {
        return None;
    }
    let records: Vec<&NamedType> = cmd
        .named_types
        .iter()
        .filter(|t| t.kind != "table" && t.kind != "packable" && t.kind != "data")
        .collect();
    // A `data` earns a block when it says more than the argument's own
    // `values:` line already does: a description of the type or of a
    // constructor, or a constructor with fields.
    let datas: Vec<&NamedType> = cmd
        .named_types
        .iter()
        .filter(|t| {
            t.kind == "data"
                && (t.desc.iter().any(|l| !l.is_empty())
                    || t.constructors.iter().any(|c| {
                        !c.fields.is_empty() || c.desc.iter().any(|l| !l.is_empty())
                    }))
        })
        .collect();
    let tables: Vec<&NamedType> =
        cmd.named_types.iter().filter(|t| t.kind == "table").collect();
    let packables: Vec<&NamedType> = cmd
        .named_types
        .iter()
        .filter(|t| t.kind == "packable")
        .collect();

    let mut out = String::new();
    if !records.is_empty() {
        out.push_str("Record Schemas:\n");
        out.push_str(&render_named(&records));
    }
    if !datas.is_empty() {
        if !out.is_empty() {
            out.push('\n');
        }
        out.push_str("Data Types:\n");
        out.push_str(&render_datas(&datas));
    }
    if !tables.is_empty() {
        if !out.is_empty() {
            out.push('\n');
        }
        out.push_str("Table Schemas:\n");
        out.push_str(&render_named(&tables));
    }
    if !packables.is_empty() {
        if !out.is_empty() {
            out.push('\n');
        }
        out.push_str("Wire Forms:\n");
        out.push_str(&render_packables(&packables));
    }
    // Drop the trailing newline so clap can append its own block
    // separator cleanly.
    if out.ends_with('\n') {
        out.pop();
    }
    Some(out)
}

/// Render `data` types, one block each: the name, its description when it
/// has one, then each constructor with its field types and the prose
/// written above it. A value is typed by the constructor's name alone
/// when it takes no fields, so that is what a reader is looking for.
fn render_datas(defs: &[&NamedType]) -> String {
    let mut out = String::new();
    for (i, def) in defs.iter().enumerate() {
        if i > 0 {
            out.push('\n');
        }
        out.push_str(&format!("  {}\n", head_of(def)));
        for line in def.desc.iter().filter(|l| !l.is_empty()) {
            out.push_str(&format!("    {}\n", line));
        }
        let heads: Vec<String> = def
            .constructors
            .iter()
            .map(|c| {
                if c.fields.is_empty() {
                    c.name.clone()
                } else {
                    let fields: Vec<String> =
                        c.fields.iter().map(|f| as_argument(f)).collect();
                    format!("{} {}", c.name, fields.join(" "))
                }
            })
            .collect();
        let width = heads.iter().map(|h| h.len()).max().unwrap_or(0);
        for (c, head) in def.constructors.iter().zip(heads.iter()) {
            let desc: Vec<&str> = c.desc.iter().map(|l| l.as_str()).filter(|l| !l.is_empty()).collect();
            if desc.is_empty() {
                out.push_str(&format!("    {}\n", head));
            } else {
                out.push_str(&format!("    {:width$}  {}\n", head, desc[0], width = width));
                for line in &desc[1..] {
                    out.push_str(&format!("    {:width$}  {}\n", "", line, width = width));
                }
            }
        }
    }
    out
}

/// A rendered type as it must be spelled in argument position: an
/// application (`Box Todo`) takes parentheses, since `Boxed Box Todo`
/// reads as two fields; a self-delimiting form (`[Todo]`, `(Int, Todo)`,
/// `{a = Int}`, `?(Box Todo)`) or a bare name does not.
///
/// The test is a space outside every `()`, `[]` and `{}` pair. That holds
/// because the compiler's type printer always separates the parts of an
/// application, an arrow or an effect row with a space and parenthesizes
/// every other compound form itself. `<` and `>` are not brackets here:
/// `->` contains one.
fn as_argument(t: &str) -> String {
    let mut depth: i32 = 0;
    let mut spaced = false;
    for ch in t.chars() {
        match ch {
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => depth -= 1,
            ' ' if depth == 0 => spaced = true,
            _ => {}
        }
    }
    if spaced {
        format!("({})", t)
    } else {
        t.to_string()
    }
}

/// A definition's head: the name followed by its type parameters, in
/// which the body is stated. `Box a` over `value :: a` says where the
/// parameter goes; a bare `Box` would leave the `a` unexplained.
fn head_of(def: &NamedType) -> String {
    if def.parameters.is_empty() {
        def.name.clone()
    } else {
        format!("{} {}", def.name, def.parameters.join(" "))
    }
}

/// Render types whose definition is a wire form rather than a field
/// list: `Name p1 p2 = <form>`, stated in the constructor's own
/// parameters so one line covers every use of it.
fn render_packables(defs: &[&NamedType]) -> String {
    let mut out = String::new();
    for def in defs {
        out.push_str(&format!("  {} = {}\n", head_of(def), def.equals));
    }
    out
}

/// Render a list of named types, one block each: the name on its own
/// line, then its fields with the `::` column aligned.
fn render_named(defs: &[&NamedType]) -> String {
    let mut out = String::new();
    for (i, def) in defs.iter().enumerate() {
        if i > 0 {
            out.push('\n');
        }
        out.push_str(&format!("  {}\n", head_of(def)));
        let width = def.fields.iter().map(|f| f.key.len()).max().unwrap_or(0);
        for f in &def.fields {
            out.push_str(&format!(
                "    {:width$} :: {}\n",
                f.key,
                f.type_desc,
                width = width
            ));
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::as_argument;

    #[test]
    fn applications_take_parentheses_in_argument_position() {
        for t in ["Box Todo", "Vector 4 Int", "Table {name = Str}", "<IO> Int", "Int -> Int"] {
            assert_eq!(as_argument(t), format!("({})", t), "{t}");
        }
    }

    #[test]
    fn atoms_and_delimited_forms_stay_bare() {
        for t in ["Todo", "a", "_", "[Box Todo]", "(Int, Todo)", "{a = Box Todo}", "?(Box Todo)", "?Todo", "(n + 1)"] {
            assert_eq!(as_argument(t), t, "{t}");
        }
    }
}
