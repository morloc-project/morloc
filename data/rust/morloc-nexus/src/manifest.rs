//! Manifest JSON deserialization.
//!
//! Replaces manifest.c / manifest.h with serde_json-based parsing.

use serde::Deserialize;

// Fields are populated by serde deserialization from manifest JSON
#[derive(Debug, Deserialize)]
#[allow(dead_code)]
pub struct Manifest {
    pub version: i32,
    #[serde(default)]
    pub name: Option<String>,
    pub build_dir: Option<String>,
    #[serde(default)]
    pub pools: Vec<Pool>,
    #[serde(default)]
    pub commands: Vec<Command>,
    #[serde(default)]
    pub groups: Vec<CmdGroup>,
    #[serde(default)]
    pub service: Option<Service>,
}

#[derive(Debug, Deserialize)]
pub struct Pool {
    pub lang: String,
    pub exec: Vec<String>,
    pub socket: String,
}

#[derive(Debug, Deserialize)]
pub struct Command {
    pub name: String,
    #[serde(rename = "type")]
    pub cmd_type: String,
    // Remote-only fields
    #[serde(default)]
    pub mid: u32,
    #[serde(default, rename = "pool")]
    pub pool_index: usize,
    #[serde(default)]
    pub needed_pools: Vec<usize>,
    // Common fields
    #[serde(default)]
    pub arg_schemas: Vec<String>,
    #[serde(default)]
    pub return_schema: String,
    #[serde(default)]
    pub desc: Vec<String>,
    #[serde(default)]
    pub return_type: String,
    #[serde(default)]
    pub return_desc: Vec<String>,
    #[serde(default)]
    pub args: Vec<Arg>,
    // Pure-only field
    #[serde(default)]
    pub expr: Option<serde_json::Value>,
    // Command group
    #[serde(default, deserialize_with = "deserialize_nullable_string")]
    pub group: Option<String>,
}

impl Command {
    pub fn is_pure(&self) -> bool {
        self.cmd_type == "pure"
    }
}

#[derive(Debug, Deserialize)]
#[serde(tag = "kind")]
pub enum Arg {
    #[serde(rename = "pos")]
    Positional {
        #[serde(default)]
        metavar: Option<String>,
        #[serde(default)]
        type_desc: Option<String>,
        #[serde(default)]
        quoted: bool,
        #[serde(default)]
        desc: Vec<String>,
    },
    #[serde(rename = "opt")]
    Optional {
        #[serde(default)]
        metavar: Option<String>,
        #[serde(default)]
        type_desc: Option<String>,
        #[serde(default)]
        quoted: bool,
        #[serde(default, rename = "short")]
        short_opt: Option<String>,
        #[serde(default, rename = "long")]
        long_opt: Option<String>,
        #[serde(default, rename = "default")]
        default_val: Option<String>,
        #[serde(default)]
        desc: Vec<String>,
    },
    #[serde(rename = "flag")]
    Flag {
        #[serde(default, rename = "short")]
        short_opt: Option<String>,
        #[serde(default, rename = "long")]
        long_opt: Option<String>,
        #[serde(default)]
        long_rev: Option<String>,
        #[serde(default, rename = "default")]
        default_val: Option<String>,
        #[serde(default)]
        desc: Vec<String>,
    },
    #[serde(rename = "grp")]
    Group {
        #[serde(default)]
        metavar: Option<String>,
        #[serde(default)]
        desc: Vec<String>,
        #[serde(default)]
        group_opt: Option<GroupOpt>,
        #[serde(default)]
        entries: Vec<GroupEntry>,
    },
}

#[derive(Debug, Deserialize)]
pub struct GroupOpt {
    #[serde(default, rename = "short")]
    pub short_opt: Option<String>,
    #[serde(default, rename = "long")]
    pub long_opt: Option<String>,
}

// Fields are populated by serde deserialization from manifest JSON
#[derive(Debug, Deserialize)]
#[allow(dead_code)]
pub struct GroupEntry {
    pub key: String,
    pub arg: Arg,
}

#[derive(Debug, Deserialize)]
pub struct CmdGroup {
    pub name: String,
    #[serde(default)]
    pub desc: Vec<String>,
}

// Fields are populated by serde deserialization from manifest JSON
#[derive(Debug, Deserialize)]
#[allow(dead_code)]
pub struct Service {
    #[serde(rename = "type")]
    pub service_type: Option<String>,
    pub host: Option<String>,
    pub port: Option<i32>,
    pub socket: Option<String>,
}

/// Read manifest payload from a file. If it starts with "#!", scan for
/// "### MANIFEST ###" marker and read after it.
pub fn read_manifest_payload(path: &str) -> Result<String, String> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| format!("Cannot open manifest file '{}': {}", path, e))?;

    if content.starts_with("#!") {
        // Wrapper script: find the manifest marker
        if let Some(pos) = content.find("### MANIFEST ###") {
            // Skip to the line after the marker
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

pub fn parse_manifest(payload: &str) -> Result<Manifest, String> {
    serde_json::from_str(payload).map_err(|e| format!("Failed to parse manifest JSON: {}", e))
}

/// Deserialize a string that may be the literal JSON `"null"` as None.
fn deserialize_nullable_string<'de, D>(deserializer: D) -> Result<Option<String>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let s: Option<String> = Option::deserialize(deserializer)?;
    match s {
        Some(ref v) if v == "null" => Ok(None),
        other => Ok(other),
    }
}

impl Arg {
    pub fn short_opt_char(&self) -> Option<char> {
        let s = match self {
            Arg::Optional { short_opt, .. } => short_opt.as_deref(),
            Arg::Flag { short_opt, .. } => short_opt.as_deref(),
            _ => None,
        };
        s.and_then(|s| s.chars().next())
    }

    pub fn long_opt_str(&self) -> Option<&str> {
        match self {
            Arg::Optional { long_opt, .. } => long_opt.as_deref(),
            Arg::Flag { long_opt, .. } => long_opt.as_deref(),
            _ => None,
        }
    }

    #[allow(dead_code)]
    pub fn is_flag(&self) -> bool {
        matches!(self, Arg::Flag { .. })
    }

    pub fn is_quoted(&self) -> bool {
        match self {
            Arg::Positional { quoted, .. } | Arg::Optional { quoted, .. } => *quoted,
            _ => false,
        }
    }

    pub fn default_val(&self) -> Option<&str> {
        match self {
            Arg::Optional { default_val, .. } => default_val.as_deref(),
            Arg::Flag { default_val, .. } => default_val.as_deref(),
            _ => None,
        }
    }

    #[allow(dead_code)] // Manifest data-model accessor; used by help formatting
    pub fn metavar_str(&self) -> Option<&str> {
        match self {
            Arg::Positional { metavar, .. } => metavar.as_deref(),
            Arg::Optional { metavar, .. } => metavar.as_deref(),
            Arg::Group { metavar, .. } => metavar.as_deref(),
            _ => None,
        }
    }

    #[allow(dead_code)] // Manifest data-model accessor; used by help formatting
    pub fn desc_lines(&self) -> &[String] {
        match self {
            Arg::Positional { desc, .. }
            | Arg::Optional { desc, .. }
            | Arg::Flag { desc, .. }
            | Arg::Group { desc, .. } => desc,
        }
    }

    #[allow(dead_code)] // Manifest data-model accessor; used by help formatting
    pub fn type_desc_str(&self) -> Option<&str> {
        match self {
            Arg::Positional { type_desc, .. } | Arg::Optional { type_desc, .. } => {
                type_desc.as_deref()
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_manifest() {
        let json = r#"{
            "version": 1,
            "pools": [
                {"lang": "python3", "exec": ["python3", "pool.py"], "socket": "pipe-python3"}
            ],
            "commands": [
                {
                    "name": "f",
                    "type": "remote",
                    "mid": 1,
                    "pool": 0,
                    "needed_pools": [0],
                    "arg_schemas": ["s"],
                    "return_schema": "s",
                    "desc": [],
                    "return_type": "Str",
                    "return_desc": [],
                    "args": [
                        {"kind": "pos", "metavar": null, "type_desc": "Str", "quoted": false, "desc": []}
                    ],
                    "group": "null"
                }
            ]
        }"#;
        let m = parse_manifest(json).unwrap();
        assert_eq!(m.version, 1);
        assert_eq!(m.pools.len(), 1);
        assert_eq!(m.pools[0].lang, "python3");
        assert_eq!(m.commands.len(), 1);
        assert_eq!(m.commands[0].name, "f");
        assert!(!m.commands[0].is_pure());
        assert_eq!(m.commands[0].mid, 1);
        assert!(m.commands[0].group.is_none()); // "null" -> None
    }

    #[test]
    fn test_parse_pure_command() {
        let json = r#"{
            "version": 1,
            "pools": [],
            "commands": [
                {
                    "name": "greet",
                    "type": "pure",
                    "arg_schemas": ["s"],
                    "return_schema": "s",
                    "desc": ["Say hello"],
                    "return_type": "Str",
                    "return_desc": [],
                    "args": [
                        {"kind": "pos", "metavar": "NAME", "type_desc": "Str", "quoted": true, "desc": ["name"]}
                    ],
                    "expr": {"tag": "lit", "schema": "s", "lit_type": "str", "value": "hello"},
                    "group": null
                }
            ]
        }"#;
        let m = parse_manifest(json).unwrap();
        assert!(m.commands[0].is_pure());
        assert!(m.commands[0].expr.is_some());
    }

    #[test]
    fn test_parse_flag_and_opt() {
        let json = r#"{
            "version": 1,
            "pools": [],
            "commands": [
                {
                    "name": "test",
                    "type": "remote",
                    "mid": 0,
                    "pool": 0,
                    "needed_pools": [],
                    "arg_schemas": [],
                    "return_schema": "z",
                    "desc": [],
                    "return_type": "Nil",
                    "return_desc": [],
                    "args": [
                        {"kind": "opt", "metavar": "FILE", "type_desc": "Str", "quoted": true, "short": "f", "long": "file", "default": "out.txt", "desc": ["output file"]},
                        {"kind": "flag", "short": "v", "long": "verbose", "long_rev": "no-verbose", "default": "false", "desc": ["enable verbose"]}
                    ],
                    "group": "null"
                }
            ]
        }"#;
        let m = parse_manifest(json).unwrap();
        let args = &m.commands[0].args;
        assert_eq!(args.len(), 2);
        assert!(matches!(args[0], Arg::Optional { .. }));
        assert!(matches!(args[1], Arg::Flag { .. }));
        assert_eq!(args[0].short_opt_char(), Some('f'));
        assert_eq!(args[1].long_opt_str(), Some("verbose"));
    }

    #[test]
    fn test_wrapper_script_extraction() {
        let content = "#!/usr/bin/env morloc-nexus\nsome stuff\n### MANIFEST ###\n{\"version\":1,\"pools\":[],\"commands\":[]}";
        // Simulate the extraction logic
        let pos = content.find("### MANIFEST ###").unwrap();
        let after = &content[pos..];
        let nl = after.find('\n').unwrap();
        let payload = &content[pos + nl + 1..];
        let m: Manifest = serde_json::from_str(payload).unwrap();
        assert_eq!(m.version, 1);
    }
}
