//! The body of a call request, read the same way by the daemon's HTTP API
//! and the serving front end: a JSON array of arguments, or an object whose
//! only member is `args`.

/// The `args` text of a call body; `None` when the body is empty.
pub fn args_of(body: &[u8]) -> Result<Option<&str>, String> {
    use serde_json::value::RawValue;
    let text = std::str::from_utf8(body).map_err(|_| "the request body is not UTF-8".to_string())?;
    let trimmed = text.trim();
    let malformed = |e: serde_json::Error| format!("malformed JSON in the request body: {}", e);
    match trimmed.as_bytes().first() {
        None => Ok(None),
        Some(b'[') => serde_json::from_str::<&RawValue>(trimmed).map(|v| Some(v.get())).map_err(malformed),
        Some(b'{') => {
            let mut args = None;
            for (key, v) in serde_json::from_str::<Envelope>(trimmed).map_err(malformed)?.0 {
                match key.as_str() {
                    "args" if args.is_none() => args = Some(v),
                    "args" => return Err("'args' given twice".into()),
                    other => return Err(format!("unknown member '{}' in the request body; arguments go under 'args'", other)),
                }
            }
            match args {
                None => Ok(None),
                Some(a) => args_value(a.get()).map(Some),
            }
        }
        _ => Err("the request body must be a JSON array of arguments or {\"args\": ...}".into()),
    }
}

/// `args` itself: an array of arguments, one per slot, or an object keyed
/// by argument names.
pub fn args_value(text: &str) -> Result<&str, String> {
    match text.trim_start().as_bytes().first() {
        Some(b'[' | b'{') => Ok(text),
        _ => Err("'args' must be an array or an object".into()),
    }
}

/// The members of a JSON object in order, duplicates kept; an error for
/// text that is not an object.
pub fn members(text: &str) -> Result<Vec<(String, &serde_json::value::RawValue)>, String> {
    serde_json::from_str::<Envelope>(text)
        .map(|m| m.0)
        .map_err(|e| format!("malformed JSON object: {}", e))
}

/// An object's members in order, duplicates kept.
struct Envelope<'a>(Vec<(String, &'a serde_json::value::RawValue)>);

impl<'de: 'a, 'a> serde::Deserialize<'de> for Envelope<'a> {
    fn deserialize<D: serde::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        struct V<'a>(std::marker::PhantomData<&'a ()>);
        impl<'de: 'a, 'a> serde::de::Visitor<'de> for V<'a> {
            type Value = Envelope<'a>;
            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                f.write_str("an object")
            }
            fn visit_map<M: serde::de::MapAccess<'de>>(self, mut m: M) -> Result<Self::Value, M::Error> {
                let mut out = Vec::new();
                while let Some((k, v)) = m.next_entry::<String, &'de serde_json::value::RawValue>()? {
                    out.push((k, v));
                }
                Ok(Envelope(out))
            }
        }
        d.deserialize_map(V(std::marker::PhantomData))
    }
}

#[cfg(test)]
mod tests {
    use super::args_of;

    #[test]
    fn a_body_is_an_array_or_an_envelope_holding_only_args() {
        assert_eq!(args_of(b""), Ok(None));
        assert_eq!(args_of(b"  "), Ok(None));
        assert_eq!(args_of(b"[1, 2]"), Ok(Some("[1, 2]")));
        assert_eq!(args_of(br#"{"args":[1]}"#), Ok(Some("[1]")));
        assert_eq!(args_of(br#"{"args":{"_1":1}}"#), Ok(Some(r#"{"_1":1}"#)));
        assert_eq!(args_of(b"{}"), Ok(None));
        assert!(args_of(br#"{"args":5}"#).is_err());
        assert!(args_of(br#"{"args":null}"#).is_err());
        assert!(args_of(br#"{"gap":5}"#).is_err());
        assert!(args_of(br#"{"args":[1],"args":[2]}"#).is_err());
        assert!(args_of(b"5").is_err());
        assert!(args_of(b"\"x\"").is_err());
        assert!(args_of(b"[1,").is_err());
        assert!(args_of(&[b'[', b'"', 0xff, b'"', b']']).is_err());
    }
}
