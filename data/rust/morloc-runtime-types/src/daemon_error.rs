//! How a daemon reply classifies a failure, shared by the daemon and the
//! serving front end so both answer one failure the same way.

pub const OK: i32 = 0;
pub const BAD_REQUEST: i32 = 1;
pub const NOT_FOUND: i32 = 2;
pub const TIMEOUT: i32 = 3;
pub const RECOVERING: i32 = 4;
/// The call could not be carried out: its pool died, or the runtime failed
/// under it. A daemon probes and recovers its pools after one.
pub const INTERNAL: i32 = 5;
/// The call failed on its own terms: it raised, returned `Err`, or its
/// result could not be written. Nothing needs recovering.
pub const FAILED: i32 = 6;
/// The request needs a credential this transport did not present.
pub const FORBIDDEN: i32 = 7;

/// Why an exposed eval is refused over HTTP without a credential, said the
/// same way by every server that offers it.
pub const EVAL_NEEDS_TOKEN: &str = "the eval capability is exposed but requires a bearer token: \
     it compiles and runs expressions the caller writes, which costs seconds to tens of \
     seconds of this container's CPU each time. Set MORLOC_MCP_TOKEN (or --auth-token), or \
     waive the requirement with MORLOC_EVAL_ALLOW_NO_AUTH=1 (or --eval-allow-no-auth).";

/// Why eval is refused by a server not started with `--eval`.
pub const EVAL_NOT_EXPOSED: &str = "eval is not exposed; start the server with --eval";

/// The HTTP status of a reply.
pub fn http_status(kind: i32, success: bool) -> i32 {
    if success {
        return 200;
    }
    match kind {
        BAD_REQUEST => 400,
        NOT_FOUND => 404,
        TIMEOUT => 408,
        RECOVERING => 503,
        FORBIDDEN => 403,
        _ => 500,
    }
}

/// The class a failed reply names on the wire.
pub fn class_name(kind: i32) -> &'static str {
    match kind {
        BAD_REQUEST => "bad-request",
        NOT_FOUND => "not-found",
        TIMEOUT => "timeout",
        RECOVERING => "recovering",
        FORBIDDEN => "forbidden",
        _ => "failure",
    }
}

/// The JSON body of an error reply of `kind`.
pub fn error_body(kind: i32, message: &str) -> String {
    serde_json::json!({ "status": "error", "class": class_name(kind), "error": message }).to_string()
}

/// The class of an error reply that has a status but no kind.
pub fn class_of_status(status: i32) -> &'static str {
    match status {
        400 => "bad-request",
        401 => "unauthorized",
        403 => "forbidden",
        404 => "not-found",
        405 => "bad-request",
        408 => "timeout",
        413 => "limit",
        503 => "recovering",
        _ => "failure",
    }
}

/// The kind a class names; a class this build does not know is a failure.
pub fn kind_of_class(class: &str) -> i32 {
    match class {
        "bad-request" => BAD_REQUEST,
        "not-found" => NOT_FOUND,
        "timeout" => TIMEOUT,
        "recovering" => RECOVERING,
        "forbidden" => FORBIDDEN,
        _ => FAILED,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_kind_survives_its_class_name() {
        for k in [BAD_REQUEST, NOT_FOUND, TIMEOUT, RECOVERING, FAILED, FORBIDDEN] {
            assert_eq!(kind_of_class(class_name(k)), k);
        }
        assert_eq!(class_name(INTERNAL), "failure");
        assert_eq!(kind_of_class("something-new"), FAILED);
    }
}
