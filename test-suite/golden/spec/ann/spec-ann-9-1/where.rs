fn rsnode() -> String {
    std::env::var("MOCK_NODE").unwrap_or_else(|_| "parent".to_string())
}

pub fn rshere(x: i64) -> String {
    format!("{}:{}rs", rsnode(), x)
}

pub fn rstag(s: &String) -> String {
    format!("rstag@{}[{}]", rsnode(), s)
}
