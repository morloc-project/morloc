//! Byte-exact pins of every walker's output over a fixed corpus.
//!
//! The corpus covers each schema shape the walkers dispatch on, recursive
//! shapes included, and the pinned values were captured from the walkers
//! as they stood before any of them was rewritten. Flat bytes, streamed
//! bytes, hashes, JSON text, msgpack bytes and sizes are all part of the
//! wire or cache contract, so a rewrite that changes one of them is a
//! wire change, not a refactor.

use crate::schema::parse_schema;

const CORPUS: &[(&str, &str)] = &[
    ("i4", "42"),
    ("s", "\"hello\""),
    ("as", "[\"a\",\"\",\"ccc\"]"),
    ("t3si4s", "[\"x\",7,\"y\"]"),
    ("aai4", "[[1,2],[],[3]]"),
    ("t2?i4s", "[null,\"n\"]"),
    ("t2?i4s", "[5,\"p\"]"),
    ("m22idj4tagsas", "{\"id\":123456789012345678901234567890,\"tags\":[\"t\"]}"),
    ("v23Nil04Cons2i4s", "\"Nil\""),
    ("v23Nil04Cons2i4s", "{\"Cons\":[1,\"a\"]}"),
    ("e21A1B", "\"B\""),
    ("&2LLm24headi84tail?^2LL", "{\"head\":1,\"tail\":{\"head\":2,\"tail\":{\"head\":3,\"tail\":null}}}"),
    ("&4Treev24Leaf04Node3i8^4Tree^4Tree", "{\"Node\":[1,\"Leaf\",{\"Node\":[2,\"Leaf\",{\"Node\":[3,\"Leaf\",\"Leaf\"]}]}]}"),
    ("&1Av23Nil05ACons2i8&1Bv15BCons2i8^1A", "{\"ACons\":[1,{\"BCons\":[2,{\"ACons\":[3,{\"BCons\":[4,\"Nil\"]}]}]}]}"),
    ("&4Rosem21vi84kidsa^4Rose", "{\"v\":1,\"kids\":[{\"v\":2,\"kids\":[{\"v\":3,\"kids\":[]}]},{\"v\":4,\"kids\":[]}]}"),
    ("a&2LLm24headi84tail?^2LL", "[{\"head\":1,\"tail\":null},{\"head\":2,\"tail\":{\"head\":3,\"tail\":null}}]"),
];

fn hex(b: &[u8]) -> String {
    b.iter().map(|x| format!("{x:02x}")).collect()
}

/// Every pinned quantity for one corpus entry, as one string.
fn report(schema_str: &str, json: &str) -> String {
    let schema = parse_schema(schema_str).unwrap();
    let ptr = crate::json::read_json_with_schema(json, &schema).unwrap();
    let flat = crate::voidstar::flatten_to_buffer(ptr, &schema).unwrap();
    let mut w0: Vec<u8> = Vec::new();
    crate::voidstar::write_flat_to_writer_with_vol_idx(&mut w0, ptr, &schema, 0).unwrap();
    let mut w7: Vec<u8> = Vec::new();
    crate::voidstar::write_flat_to_writer_with_vol_idx(&mut w7, ptr, &schema, 7).unwrap();
    let h0 = crate::cache::hash_voidstar_value(ptr, &schema, 0).unwrap();
    let h17 = crate::cache::hash_voidstar_value(ptr, &schema, 17).unwrap();
    let js = crate::json::voidstar_to_json_string(ptr, &schema).unwrap();
    let pretty = crate::json::pretty_json_string(ptr, &schema).unwrap();
    let mpk = crate::mpack::pack_with_schema(ptr, &schema).unwrap();
    let size = crate::ffi::calc_voidstar_size_inner(ptr, &schema).unwrap();
    let unpack = crate::mpack::calc_unpack_size(&mpk, &schema).unwrap();
    format!(
        "flat={} w0={} w7={} h0={h0:016x} h17={h17:016x} json={js} pretty={:?} mpk={} size={size} unpack={unpack}",
        hex(&flat), hex(&w0), hex(&w7), pretty, hex(&mpk)
    )
}

#[test]
fn walker_outputs_match_the_pins() {
    let _shm = crate::init_test_shm();
    let mut bad = Vec::new();
    for ((schema, json), expect) in CORPUS.iter().zip(PINS.iter()) {
        let got = report(schema, json);
        if got != *expect {
            bad.push(format!("{schema} {json}\n  expected {expect}\n  got      {got}"));
        }
    }
    assert!(bad.is_empty(), "{}", bad.join("\n"));
    assert_eq!(CORPUS.len(), PINS.len());
}

/// Prints the report for every corpus entry, for capturing new pins when
/// the corpus grows.
#[test]
#[ignore]
fn print_pins() {
    let _shm = crate::init_test_shm();
    for (schema, json) in CORPUS {
        println!("    {:?},", report(schema, json));
    }
}

const PINS: &[&str] = &[
    "flat=2a000000 w0=2a000000 w7=2a000000 h0=28c71650acc0b031 h17=6d50ed455689a42f json=42 pretty=\"42\" mpk=2a size=4 unpack=4",
    "flat=0500000000000000100000000000000068656c6c6f w0=0500000000000000100000000000000068656c6c6f w7=0500000000000000100000000000070068656c6c6f h0=447c3bd1a33eb4df h17=9f8d8d05b6b0b00f json=\"hello\" pretty=\"\\\"hello\\\"\" mpk=a568656c6c6f size=21 unpack=21",
    "flat=030000000000000010000000000000000100000000000000400000000000000000000000000000000000000000000000030000000000000041000000000000006163636300000000000000 w0=0300000000000000100000000000000001000000000000004000000000000000000000000000000000000000000000000300000000000000410000000000000061636363 w7=0300000000000000100000000000070001000000000000004000000000000700000000000000000000000000000000000300000000000000410000000000070061636363 h0=68f3ea638c3a7301 h17=49f71b8141b41b75 json=[\"a\",\"\",\"ccc\"] pretty=\"[\\n  \\\"a\\\",\\n  \\\"\\\",\\n  \\\"ccc\\\"\\n]\" mpk=93a161a0a3636363 size=75 unpack=68",
    "flat=010000000000000028000000000000000700000000000000010000000000000029000000000000007879 w0=010000000000000028000000000000000700000000000000010000000000000029000000000000007879 w7=010000000000000028000000000007000700000000000000010000000000000029000000000007007879 h0=137fa654585cc32c h17=017fb447bedd18ce json=[\"x\",7,\"y\"] pretty=\"[\\n  \\\"x\\\",\\n  7,\\n  \\\"y\\\"\\n]\" mpk=93a17807a179 size=42 unpack=74",
    "flat=0300000000000000100000000000000002000000000000004000000000000000000000000000000000000000000000000100000000000000800000000000000001000000020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000030000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 w0=030000000000000010000000000000000200000000000000400000000000000000000000000000000000000000000000010000000000000080000000000000000100000002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003000000 w7=030000000000000010000000000007000200000000000000400000000000070000000000000000000000000000000000010000000000000080000000000007000100000002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003000000 h0=cb0d772bab550929 h17=261cb90fd8c2f5db json=[[1,2],[],[3]] pretty=\"[\\n  [\\n    1,\\n    2\\n  ],\\n  [],\\n  [\\n    3\\n  ]\\n]\" mpk=93920102909103 size=209 unpack=220",
    "flat=ffffffffffffffff010000000000000018000000000000006e w0=ffffffffffffffff010000000000000018000000000000006e w7=ffffffffffffffff010000000000000018000000000007006e h0=a12b2e6e0d9d08d6 h17=2b296102cccdfdbe json=[null,\"n\"] pretty=\"[\\n  null,\\n  \\\"n\\\"\\n]\" mpk=92c0a16e size=25 unpack=49",
    "flat=180000000000000001000000000000001c000000000000000500000070000000 w0=180000000000000001000000000000001c000000000000000500000070 w7=180000000000070001000000000000001c000000000007000500000070 h0=d4a8f2bd0361776e h17=2f1aa9fa3c538731 json=[5,\"p\"] pretty=\"[\\n  5,\\n  \\\"p\\\"\\n]\" mpk=9205a170 size=32 unpack=56",
    "flat=0200000000000000200000000000000001000000000000003000000000000000d20a3f4eeee073c3f60fe98e0100000001000000000000004000000000000000740000000000000000000000000000 w0=0200000000000000200000000000000001000000000000003000000000000000d20a3f4eeee073c3f60fe98e010000000100000000000000400000000000000074 w7=0200000000000000200000000000070001000000000000003000000000000700d20a3f4eeee073c3f60fe98e010000000100000000000000400000000000070074 h0=e591c10a19518b6b h17=33af21b71dc0792a json={\"id\":123456789012345678901234567890,\"tags\":[\"t\"]} pretty=\"{\\n  \\\"id\\\": 123456789012345678901234567890,\\n  \\\"tags\\\": [\\n    \\\"t\\\"\\n  ]\\n}\" mpk=92c410d20a3f4eeee073c3f60fe98e0100000091a174 size=79 unpack=104",
    "flat=0000000000000000ffffffffffffffff w0=0000000000000000ffffffffffffffff w7=0000000000000000ffffffffffffffff h0=ed04727b7bd58149 h17=c2d2d66c6104de21 json=\"Nil\" pretty=\"\\\"Nil\\\"\" mpk=9200c0 size=16 unpack=16",
    "flat=010000000000000010000000000000000100000000000000010000000000000028000000000000006100000000000000 w0=0100000000000000100000000000000001000000000000000100000000000000280000000000000061 w7=0100000000000000100000000000070001000000000000000100000000000000280000000000070061 h0=237a20129f2b5bf3 h17=1e7e57f16b5b9f33 json={\"Cons\":[1,\"a\"]} pretty=\"{\\\"Cons\\\":[\\n  1,\\n  \\\"a\\\"\\n]}\" mpk=92019201a161 size=48 unpack=64",
    "flat=01 w0=01 w7=01 h0=6f5d1428e0810667 h17=77153121a48eafdc json=\"B\" pretty=\"\\\"B\\\"\" mpk=01 size=1 unpack=1",
    "flat=01000000000000001000000000000000020000000000000020000000000000000300000000000000ffffffffffffffff0000000000000000000000000000 w0=01000000000000001000000000000000020000000000000020000000000000000300000000000000ffffffffffffffff w7=01000000000000001000000000000700020000000000000020000000000007000300000000000000ffffffffffffffff h0=9b3af415e605d41a h17=a6ed51f6b0aba0bc json={\"head\":1,\"tail\":{\"head\":2,\"tail\":{\"head\":3,\"tail\":null}}} pretty=\"{\\n  \\\"head\\\": 1,\\n  \\\"tail\\\": {\\n    \\\"head\\\": 2,\\n    \\\"tail\\\": {\\n      \\\"head\\\": 3,\\n      \\\"tail\\\": null\\n    }\\n  }\\n}\" mpk=920192029203c0 size=62 unpack=86",
    "flat=0100000000000000100000000000000001000000000000000000000000000000ffffffffffffffff0100000000000000380000000000000002000000000000000000000000000000ffffffffffffffff0100000000000000600000000000000003000000000000000000000000000000ffffffffffffffff0000000000000000ffffffffffffffff000000000000000000000000000000000000000000 w0=0100000000000000100000000000000001000000000000000000000000000000ffffffffffffffff0100000000000000380000000000000002000000000000000000000000000000ffffffffffffffff0100000000000000600000000000000003000000000000000000000000000000ffffffffffffffff0000000000000000ffffffffffffffff w7=0100000000000000100000000000070001000000000000000000000000000000ffffffffffffffff0100000000000000380000000000070002000000000000000000000000000000ffffffffffffffff0100000000000000600000000000070003000000000000000000000000000000ffffffffffffffff0000000000000000ffffffffffffffff h0=d90f89da75d681f5 h17=e5238983ea4ce304 json={\"Node\":[1,\"Leaf\",{\"Node\":[2,\"Leaf\",{\"Node\":[3,\"Leaf\",\"Leaf\"]}]}]} pretty=\"{\\\"Node\\\":[\\n  1,\\n  \\\"Leaf\\\",\\n  {\\\"Node\\\":[\\n    2,\\n    \\\"Leaf\\\",\\n    {\\\"Node\\\":[\\n      3,\\n      \\\"Leaf\\\",\\n      \\\"Leaf\\\"\\n    ]}\\n  ]}\\n]}\" mpk=920193019200c0920193029200c0920193039200c09200c0 size=157 unpack=253",
    "flat=0100000000000000100000000000000001000000000000000000000000000000280000000000000002000000000000000100000000000000400000000000000003000000000000000000000000000000580000000000000004000000000000000000000000000000ffffffffffffffff00000000000000000000000000000000000000000000000000000000 w0=0100000000000000100000000000000001000000000000000000000000000000280000000000000002000000000000000100000000000000400000000000000003000000000000000000000000000000580000000000000004000000000000000000000000000000ffffffffffffffff w7=0100000000000000100000000000070001000000000000000000000000000000280000000000070002000000000000000100000000000000400000000000070003000000000000000000000000000000580000000000070004000000000000000000000000000000ffffffffffffffff h0=a024ea4dcdd72173 h17=6523d75325ff7154 json={\"ACons\":[1,{\"BCons\":[2,{\"ACons\":[3,{\"BCons\":[4,\"Nil\"]}]}]}]} pretty=\"{\\\"ACons\\\":[\\n  1,\\n  {\\\"BCons\\\":[\\n    2,\\n    {\\\"ACons\\\":[\\n      3,\\n      {\\\"BCons\\\":[\\n        4,\\n        \\\"Nil\\\"\\n      ]}\\n    ]}\\n  ]}\\n]}\" mpk=920192019200920292019203920092049200c0 size=140 unpack=204",
    "flat=0100000000000000020000000000000018000000000000000200000000000000010000000000000048000000000000000400000000000000000000000000000000000000000000000300000000000000000000000000000000000000000000000000000000000000000000000000 w0=010000000000000002000000000000001800000000000000020000000000000001000000000000004800000000000000040000000000000000000000000000000000000000000000030000000000000000000000000000000000000000000000 w7=010000000000000002000000000000001800000000000700020000000000000001000000000000004800000000000700040000000000000000000000000000000000000000000000030000000000000000000000000000000000000000000000 h0=548996d2882a4c8a h17=581b7923ed4d5f2b json={\"v\":1,\"kids\":[{\"v\":2,\"kids\":[{\"v\":3,\"kids\":[]}]},{\"v\":4,\"kids\":[]}]} pretty=\"{\\n  \\\"v\\\": 1,\\n  \\\"kids\\\": [\\n    {\\n      \\\"v\\\": 2,\\n      \\\"kids\\\": [\\n        {\\n          \\\"v\\\": 3,\\n          \\\"kids\\\": []\\n        }\\n      ]\\n    },\\n    {\\n      \\\"v\\\": 4,\\n      \\\"kids\\\": []\\n    }\\n  ]\\n}\" mpk=920192920291920390920490 size=110 unpack=160",
    "flat=020000000000000010000000000000000100000000000000ffffffffffffffff020000000000000030000000000000000300000000000000ffffffffffffffff0000000000000000000000000000 w0=020000000000000010000000000000000100000000000000ffffffffffffffff020000000000000030000000000000000300000000000000ffffffffffffffff w7=020000000000000010000000000007000100000000000000ffffffffffffffff020000000000000030000000000007000300000000000000ffffffffffffffff h0=3849deec2b7fad55 h17=44d921ec7f704d74 json=[{\"head\":1,\"tail\":null},{\"head\":2,\"tail\":{\"head\":3,\"tail\":null}}] pretty=\"[\\n  {\\n    \\\"head\\\": 1,\\n    \\\"tail\\\": null\\n  },\\n  {\\n    \\\"head\\\": 2,\\n    \\\"tail\\\": {\\n      \\\"head\\\": 3,\\n      \\\"tail\\\": null\\n    }\\n  }\\n]\" mpk=929201c092029203c0 size=78 unpack=95",
];

/// Timing of the size walk over a large list of strings, the shape every
/// crossing of a text-heavy value pays for.
#[test]
#[ignore]
fn time_size_walk() {
    let _shm = crate::init_test_shm();
    let n = 200_000;
    let json = format!("[{}]", vec!["\"abcdefgh\""; n].join(","));
    let schema = parse_schema("as").unwrap();
    let ptr = crate::json::read_json_with_schema(&json, &schema).unwrap();
    let t = std::time::Instant::now();
    let mut total = 0;
    for _ in 0..20 {
        total += crate::ffi::calc_voidstar_size_inner(ptr, &schema).unwrap();
    }
    let per = t.elapsed().as_nanos() as f64 / (20.0 * n as f64);
    println!("size walk: {per:.1} ns per element (total {total})");
}
