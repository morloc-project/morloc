/// Whether an `Authorization` header value carries exactly `token` as a
/// bearer token. The comparison takes the same time wherever the first
/// mismatch is; only the length is compared first, and it is not secret.
pub fn authorizes(authorization: Option<&str>, token: &str) -> bool {
    match authorization.and_then(|v| v.trim().strip_prefix("Bearer ")) {
        Some(t) => constant_time_eq(t.trim().as_bytes(), token.as_bytes()),
        None => false,
    }
}

fn constant_time_eq(a: &[u8], b: &[u8]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    a.iter().zip(b).fold(0u8, |acc, (x, y)| acc | (x ^ y)) == 0
}

#[cfg(test)]
mod tests {
    use super::authorizes;

    #[test]
    fn only_the_exact_bearer_token_authorizes() {
        assert!(authorizes(Some("Bearer s3cret"), "s3cret"));
        assert!(authorizes(Some("  Bearer s3cret \r"), "s3cret"));
        assert!(!authorizes(Some("Bearer nope"), "s3cret"));
        assert!(!authorizes(Some("Bearer s3cre"), "s3cret"));
        assert!(!authorizes(Some("Basic s3cret"), "s3cret"));
        assert!(!authorizes(Some("s3cret"), "s3cret"));
        assert!(!authorizes(None, "s3cret"));
    }
}
