/// The `render` query parameter of a `/call`: absent, or named once.
pub fn render_selection(query: &str) -> Result<Option<&str>, &'static str> {
    let mut found = query.split('&').filter_map(|pair| match pair.split_once('=') {
        Some(("render", v)) => Some(v),
        _ => None,
    });
    match (found.next(), found.next()) {
        (None, _) => Ok(None),
        (Some(v), None) => Ok(Some(v)),
        (Some(_), Some(_)) => Err("'render' given more than once"),
    }
}

#[cfg(test)]
mod tests {
    use super::render_selection;

    #[test]
    fn render_is_absent_named_once_or_refused() {
        assert_eq!(render_selection(""), Ok(None));
        assert_eq!(render_selection("x=1"), Ok(None));
        assert_eq!(render_selection("render=png"), Ok(Some("png")));
        assert_eq!(render_selection("x=1&render=raw"), Ok(Some("raw")));
        assert!(render_selection("render=a&render=b").is_err());
    }
}
