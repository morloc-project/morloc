//! The runtime pattern check agrees with the compiler's (PAT-5) on the
//! table the compiler's test suite also reads (test-suite/SpecTests.hs,
//! spec-pat-9-1).

use morloc_runtime_types::pattern::{check_pattern_against_schema, parse_pattern};
use morloc_runtime_types::schema::parse_schema;

#[test]
fn runtime_pattern_check_agrees_with_the_compiler() {
    let table = include_str!("pattern-conformance.tsv");
    let mut wrong = Vec::new();
    let mut n = 0;
    for line in table.lines().filter(|l| !l.is_empty() && !l.starts_with('#')) {
        let cols: Vec<&str> = line.split('\t').collect();
        let [pattern, ty, schema, expected] = cols[..] else {
            panic!("malformed row {line:?}");
        };
        let path = parse_pattern(pattern).unwrap_or_else(|e| panic!("{pattern:?} does not parse: {e}"));
        let parsed = parse_schema(schema).unwrap_or_else(|e| panic!("{schema:?} does not parse: {e}"));
        let accepted = if check_pattern_against_schema(&path, &parsed).is_ok() { "yes" } else { "no" };
        if accepted != expected {
            wrong.push(format!("{pattern} on {ty} ({schema}): runtime says {accepted}, compiler {expected}"));
        }
        n += 1;
    }
    assert!(n > 10, "table has only {n} rows");
    assert!(wrong.is_empty(), "{}", wrong.join("\n"));
}
