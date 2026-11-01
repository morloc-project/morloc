//! The arguments of a call made off the command line. A supplied argument is
//! its JSON value (IFACE-4); an omitted one takes the value the command line
//! gives it, its default read by the command line's rules (IFACE-5). The
//! arguments come as the positional array, one slot per argument, or as an
//! object keyed by argument names.

use crate::cli_value::{self, ArgShape};
use crate::{Arg, Command};
use serde_json::value::RawValue;

/// How the runtime reads a default's text: the shaped parser's codes and
/// per-element configuration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Shape {
    pub source: u8,
    pub form: u8,
    pub list_config: Option<String>,
}

/// One argument slot, ready for the runtime's argument parser.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CallArg {
    /// Text read by the classifier, or by `shape` when it is set.
    Value { text: String, shape: Option<Shape>, supplied: bool },
    /// A record: the whole value if one was given, each field given on its
    /// own, and each field's default, in entry order.
    Group { bundle: Option<String>, fields: Vec<Option<String>>, defaults: Vec<Option<String>> },
}

/// True when a call off the command line must give the argument. A
/// positional that may be read from stdin is required there (IFACE-3).
pub fn required_off_cli(arg: &Arg) -> bool {
    match arg {
        Arg::Positional { schema, stdin, .. } => *stdin || !schema_is_optional(schema.as_deref()),
        Arg::Alt { required, .. } => *required,
        Arg::Optional { .. } | Arg::Flag { .. } | Arg::Group { .. } => false,
    }
}

pub fn schema_is_optional(schema: Option<&str>) -> bool {
    use morloc_runtime_types::schema::{parse_schema, SerialType};
    schema
        .and_then(|s| parse_schema(s).ok())
        .is_some_and(|p| p.serial_type == SerialType::Optional)
}

/// The value of a flag left out: its declared default, false when none.
pub fn flag_default(default_val: Option<&str>) -> bool {
    matches!(default_val, Some("true" | "1"))
}

/// What a name in the object form addresses.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Target {
    Slot(usize),
    Bundle(usize),
    Field(usize, usize),
}

/// The names that address exactly one argument of `cmd`; a name two
/// arguments share is refused by the object form.
pub fn unambiguous_names(cmd: &Command) -> std::collections::HashSet<String> {
    let names = argument_names(cmd);
    names
        .iter()
        .filter(|(n, _)| names.iter().filter(|(m, _)| m == n).count() == 1)
        .map(|(n, _)| n.clone())
        .collect()
}

/// Every name the object form accepts, in declaration order. A group is
/// named whole only when its record can be given whole on the command line;
/// its fields are always named on their own.
fn argument_names(cmd: &Command) -> Vec<(String, Target)> {
    let mut out = Vec::new();
    for (i, arg) in cmd.args.iter().enumerate() {
        match arg {
            Arg::Group { key, group_opt, entries, .. } => {
                if group_opt.is_some() {
                    out.push((key.clone(), Target::Bundle(i)));
                }
                for (j, e) in entries.iter().enumerate() {
                    out.push((e.key.clone(), Target::Field(i, j)));
                }
            }
            _ => out.push((arg.key().to_string(), Target::Slot(i))),
        }
    }
    out
}

/// Why a call's arguments could not be resolved: the request was wrong, or
/// a default the program declares could not be read.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolveError {
    Request(String),
    Default(String),
}

impl std::fmt::Display for ResolveError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ResolveError::Request(m) | ResolveError::Default(m) => f.write_str(m),
        }
    }
}

/// Resolve a call's `args` (an array or an object; absent means none given)
/// into one [`CallArg`] per argument of `cmd`.
pub fn resolve(cmd: &Command, args: Option<&str>) -> Result<Vec<CallArg>, ResolveError> {
    let request = ResolveError::Request;
    let text = args.map(str::trim);
    match text.and_then(|t| t.as_bytes().first().copied()) {
        None => resolve_named(cmd, Vec::new()),
        Some(b'[') => {
            let values: Vec<&RawValue> = serde_json::from_str(text.unwrap_or_default())
                .map_err(|e| request(format!("malformed arguments: {}", e)))?;
            resolve_positional(cmd, values).map_err(request)
        }
        Some(b'{') => {
            let given = morloc_runtime_types::call_body::members(text.unwrap_or_default()).map_err(request)?;
            resolve_named(cmd, given)
        }
        Some(_) => Err(request("'args' must be an array or an object".into())),
    }
}

fn resolve_positional(cmd: &Command, values: Vec<&RawValue>) -> Result<Vec<CallArg>, String> {
    if values.len() != cmd.args.len() {
        return Err(format!("Expected {} arguments, got {}", cmd.args.len(), values.len()));
    }
    cmd.args
        .iter()
        .zip(values)
        .map(|(arg, v)| match arg {
            Arg::Flag { .. } => Ok(supplied(bool_text(arg.key(), v)?)),
            Arg::Group { entries, .. } => Ok(CallArg::Group {
                bundle: Some(v.get().to_string()),
                fields: vec![None; entries.len()],
                defaults: entries.iter().map(|e| e.arg.default_val().map(str::to_string)).collect(),
            }),
            _ => Ok(supplied(v.get().to_string())),
        })
        .collect()
}

fn resolve_named(cmd: &Command, given: Vec<(String, &RawValue)>) -> Result<Vec<CallArg>, ResolveError> {
    let request = ResolveError::Request;
    let names = argument_names(cmd);
    let mut values: Vec<Option<&RawValue>> = vec![None; names.len()];
    for (key, v) in given {
        let mut hits = names.iter().enumerate().filter(|(_, (n, _))| *n == key);
        let Some((k, _)) = hits.next() else {
            return Err(request(format!("unknown argument '{}'", key)));
        };
        if hits.next().is_some() {
            return Err(request(format!(
                "argument name '{}' names two arguments of '{}'; pass the arguments as an array",
                key, cmd.name
            )));
        }
        if values[k].replace(v).is_some() {
            return Err(request(format!("argument '{}' given twice", key)));
        }
    }
    let lookup = |t: Target| names.iter().position(|(_, n)| *n == t).and_then(|k| values[k]);

    let mut out = Vec::with_capacity(cmd.args.len());
    for (i, arg) in cmd.args.iter().enumerate() {
        let resolved = match arg {
            Arg::Group { entries, .. } => {
                let mut fields = Vec::with_capacity(entries.len());
                for (j, e) in entries.iter().enumerate() {
                    fields.push(match lookup(Target::Field(i, j)) {
                        None => None,
                        Some(v) if e.arg.is_flag() => Some(bool_text(&e.key, v).map_err(request)?),
                        Some(v) => Some(v.get().to_string()),
                    });
                }
                CallArg::Group {
                    bundle: lookup(Target::Bundle(i)).map(|v| v.get().to_string()),
                    fields,
                    defaults: entries.iter().map(|e| e.arg.default_val().map(str::to_string)).collect(),
                }
            }
            _ => match lookup(Target::Slot(i)) {
                Some(v) if arg.is_flag() => supplied(bool_text(arg.key(), v).map_err(request)?),
                Some(v) => supplied(v.get().to_string()),
                None if required_off_cli(arg) => {
                    return Err(request(format!("missing required argument '{}'", arg.key())));
                }
                None => omitted(arg).map_err(ResolveError::Default)?,
            },
        };
        out.push(resolved);
    }
    Ok(out)
}

/// The value the command line gives an argument left out.
fn omitted(arg: &Arg) -> Result<CallArg, String> {
    let unshaped = |text: &str| CallArg::Value { text: text.to_string(), shape: None, supplied: false };
    Ok(match arg {
        Arg::Flag { default_val, .. } => {
            unshaped(if flag_default(default_val.as_deref()) { "true" } else { "false" })
        }
        Arg::Optional { default_val: Some(d), .. } => {
            let schema = arg.schema_str().unwrap_or("");
            let is_float = morloc_runtime_types::schema::parse_schema(schema)
                .is_ok_and(|s| cli_value::is_float_scalar(&s));
            let raw = if is_float {
                cli_value::maybe_float_special_to_json(d).unwrap_or_else(|| d.clone())
            } else {
                d.clone()
            };
            let shape = ArgShape::from_arg(arg);
            let text = cli_value::shape_argv(raw, shape.as_ref())
                .map_err(|e| format!("the default of '{}': {}", arg.key(), e))?;
            let shape = shape.as_ref().and_then(|sh| {
                cli_value::shaped_dispatch_codes(sh).map(|(source, form)| Shape {
                    source,
                    form,
                    list_config: cli_value::build_list_config_json(sh),
                })
            });
            CallArg::Value { text, shape, supplied: false }
        }
        Arg::Alt { default_val: Some(d), .. } => unshaped(d),
        _ => unshaped("null"),
    })
}

fn supplied(text: String) -> CallArg {
    CallArg::Value { text, shape: None, supplied: true }
}

fn bool_text(key: &str, v: &RawValue) -> Result<String, String> {
    match serde_json::from_str::<bool>(v.get()) {
        Ok(b) => Ok(b.to_string()),
        Err(_) => Err(format!("argument '{}' must be a boolean", key)),
    }
}

/// Where the first NUL inside a string of the call's arguments is, as
/// `args[i]` and a path into the value. A supplied argument is JSON and must
/// scan; a default may be a bare word, which cannot hold a NUL, and is
/// skipped when it does not scan, as on the command line.
pub fn first_null(args: &[CallArg]) -> Result<Option<String>, String> {
    use morloc_runtime_types::null_check::first_null_in_json_text;
    let scan = |text: &str, strict: bool| match first_null_in_json_text(text) {
        Ok(p) => Ok(p),
        Err(_) if !strict => Ok(None),
        Err(e) => Err(e),
    };
    for (i, arg) in args.iter().enumerate() {
        let found = match arg {
            CallArg::Value { text, supplied, .. } => scan(text, *supplied)?,
            CallArg::Group { bundle, fields, defaults } => {
                let mut found = None;
                for (text, strict) in bundle.iter().map(|b| (b, true))
                    .chain(fields.iter().flatten().map(|f| (f, true)))
                    .chain(defaults.iter().flatten().map(|d| (d, false)))
                {
                    if let Some(p) = scan(text, strict)? {
                        found = Some(p);
                        break;
                    }
                }
                found
            }
        };
        if let Some(p) = found {
            return Ok(Some(format!("args[{}]{}", i, p)));
        }
    }
    Ok(None)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn cmd(args: &str) -> Command {
        serde_json::from_str(&format!(r#"{{"name":"f","type":"remote","args":[{}]}}"#, args)).unwrap()
    }

    const POS: &str = r#"{"kind":"pos","key":"_1","schema":"i4"}"#;
    const OPT_POS: &str = r#"{"kind":"pos","key":"_2","schema":"?i4"}"#;
    const STDIN_POS: &str = r#"{"kind":"pos","key":"_1","schema":"?i4","stdin":true}"#;
    const GAP: &str = r#"{"kind":"opt","key":"gap","schema":"i4","long":"gap","default":"5"}"#;
    const RATE: &str = r#"{"kind":"opt","key":"rate","schema":"f8","long":"rate","default":"\\-inf"}"#;
    const ON: &str = r#"{"kind":"flag","key":"on","long":"on","default":"true"}"#;
    const OFF: &str = r#"{"kind":"flag","key":"off","long":"off"}"#;
    const CFG: &str = r#"{"kind":"grp","key":"cfg","schema":"m2i4a:i4b","group_opt":{"long":"cfg"},
        "entries":[{"key":"a","arg":{"kind":"opt","key":"a","long":"a","default":"1"}},
                   {"key":"b","arg":{"kind":"flag","key":"b","long":"b"}}]}"#;

    fn value(text: &str, supplied: bool) -> CallArg {
        CallArg::Value { text: text.into(), shape: None, supplied }
    }

    #[test]
    fn an_omitted_argument_takes_the_value_the_command_line_gives_it() {
        let c = cmd(&[POS, OPT_POS, GAP, RATE, ON, OFF].join(","));
        let got = resolve(&c, Some(r#"{"_1": 3}"#)).unwrap();
        assert_eq!(got, vec![
            value("3", true),
            value("null", false),
            value("5", false),
            value("\"-inf\"", false),
            value("true", false),
            value("false", false),
        ]);
        let got = resolve(&c, Some(r#"{"_1":3,"gap":[7],"off":true,"on":false,"_2":null}"#)).unwrap();
        assert_eq!(got[1], value("null", true));
        assert_eq!(got[2], value("[7]", true));
        assert_eq!(got[4], value("false", true));
        assert_eq!(got[5], value("true", true));
    }

    #[test]
    fn a_named_call_refuses_what_the_command_line_would_not_take() {
        let c = cmd(&[POS, ON].join(","));
        assert_eq!(resolve(&c, Some("{}")).unwrap_err(), ResolveError::Request("missing required argument '_1'".into()));
        assert_eq!(resolve(&c, None).unwrap_err().to_string(), "missing required argument '_1'");
        assert!(resolve(&c, Some(r#"{"_1":1,"nope":2}"#)).unwrap_err().to_string().contains("unknown argument 'nope'"));
        assert!(resolve(&c, Some(r#"{"_1":1,"_1":2}"#)).unwrap_err().to_string().contains("given twice"));
        assert!(resolve(&c, Some(r#"{"_1":1,"on":"yes"}"#)).unwrap_err().to_string().contains("must be a boolean"));
        assert!(resolve(&c, Some("5")).is_err());
        assert!(resolve(&c, Some("{")).is_err());
    }

    #[test]
    fn a_stdin_positional_is_required_off_the_command_line() {
        let c = cmd(STDIN_POS);
        assert!(resolve(&c, Some("{}")).unwrap_err().to_string().contains("missing required argument '_1'"));
        assert_eq!(resolve(&c, Some(r#"{"_1":4}"#)).unwrap(), vec![value("4", true)]);
    }

    #[test]
    fn the_positional_form_fills_every_slot() {
        let c = cmd(&[POS, ON, CFG].join(","));
        let got = resolve(&c, Some(r#"[1, false, {"a":2}]"#)).unwrap();
        assert_eq!(got[0], value("1", true));
        assert_eq!(got[1], value("false", true));
        assert_eq!(got[2], CallArg::Group {
            bundle: Some(r#"{"a":2}"#.into()),
            fields: vec![None, None],
            defaults: vec![Some("1".into()), None],
        });
        assert!(resolve(&c, Some("[1]")).unwrap_err().to_string().contains("Expected 3 arguments, got 1"));
        assert!(resolve(&c, Some(r#"[1, 0, {}]"#)).unwrap_err().to_string().contains("must be a boolean"));
    }

    #[test]
    fn a_group_is_named_whole_or_by_its_fields() {
        let c = cmd(CFG);
        let got = resolve(&c, Some(r#"{"cfg":{"a":9},"b":true}"#)).unwrap();
        assert_eq!(got, vec![CallArg::Group {
            bundle: Some(r#"{"a":9}"#.into()),
            fields: vec![None, Some("true".into())],
            defaults: vec![Some("1".into()), None],
        }]);
        let unrolled = cmd(&CFG.replace(r#""group_opt":{"long":"cfg"},"#, ""));
        assert!(resolve(&unrolled, Some(r#"{"cfg":{}}"#)).unwrap_err().to_string().contains("unknown argument 'cfg'"));
        assert!(resolve(&unrolled, Some(r#"{"a":3}"#)).is_ok());
    }

    #[test]
    fn a_name_two_arguments_share_is_refused_by_name_only() {
        let c = cmd(&[GAP, &CFG.replace(r#""key":"a","arg""#, r#""key":"gap","arg""#)].join(","));
        assert!(resolve(&c, Some(r#"{"gap":1}"#)).unwrap_err().to_string().contains("names two arguments"));
        assert!(resolve(&c, Some("{}")).is_ok());
    }

    #[test]
    fn a_shaped_default_is_read_by_its_shape() {
        let c = cmd(r#"{"kind":"opt","key":"cfg","schema":"i4","long":"cfg","default":"x.json","source":"file"}"#);
        match &resolve(&c, None).unwrap()[0] {
            CallArg::Value { text, shape: Some(s), supplied: false } => {
                assert_eq!(text, "x.json");
                assert_eq!((s.source, s.form), (cli_value::source_atom_code(crate::SourceAtom::File), 0));
            }
            other => panic!("{:?}", other),
        }
    }

    #[test]
    fn a_nul_is_found_in_what_was_given_and_a_bare_default_is_skipped() {
        let given = vec![value("\"a\\u0000\"", true)];
        assert!(first_null(&given).unwrap().unwrap().starts_with("args[0]"));
        assert_eq!(first_null(&[value("plain words", false)]), Ok(None));
        assert!(first_null(&[value("plain words", true)]).is_err());
        let group = CallArg::Group { bundle: None, fields: vec![Some("\"\\u0000\"".into())], defaults: vec![None] };
        assert!(first_null(&[value("1", true), group]).unwrap().unwrap().starts_with("args[1]"));
    }

    #[test]
    fn a_default_the_program_cannot_read_is_not_the_callers_fault() {
        let c = cmd(r#"{"kind":"opt","key":"dna","schema":"au1","long":"dna","default":"\\q","source":"inline","form":"bytes"}"#);
        assert!(matches!(resolve(&c, None), Err(ResolveError::Default(_))));
    }
}
