//! Every schema the compiler's printer emits parses, and prints back as its
//! hint-free form. The corpus is written by the compiler's test suite
//! (test-suite/SchemaHintTests.hs).

use morloc_runtime_types::schema::{parse_schema, schema_to_string};

#[test]
fn every_printed_schema_parses_and_reprints() {
    let corpus = include_str!("schema-corpus.txt");
    let mut n = 0;
    for line in corpus.lines() {
        let (concrete, general) = line.split_once('\t').expect("concrete<TAB>general");
        for (form, text) in [("concrete", concrete), ("general", general)] {
            let parsed = parse_schema(text)
                .unwrap_or_else(|e| panic!("{form} schema {text:?} does not parse: {e}"));
            assert_eq!(schema_to_string(&parsed), general, "{form} schema {text:?}");
        }
        n += 1;
    }
    assert!(n > 100, "corpus has only {n} lines");
}
