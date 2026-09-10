// The native half of the shared library. `Action` and `Level` carry no
// per-language declaration, so the compiler emits them into this crate and
// they are simply named here. The records are declared here because their
// morloc declarations map them to these names.
#[derive(Clone)]
pub struct Step {
    pub n: i64,
    pub action: Action,
}

#[derive(Clone)]
pub struct Plan {
    pub label: String,
    pub first: Action,
    pub level: Level,
}

pub fn rs_describe(a: &Action) -> String {
    match a {
        Action::Mkdir(b) => format!("mkdir {}", b.0),
        Action::Rename(b) => format!("rename {} {}", b.0, b.1),
        Action::Chmod(b) => format!("chmod {} {}", b.0, b.1),
        Action::Noop => "noop".to_string(),
    }
}

pub fn rs_step_desc(s: &Step) -> String {
    format!("{}:{}", s.n, rs_describe(&s.action))
}

pub fn rs_plan_desc(p: &Plan) -> String {
    let lv = match p.level {
        Level::Info => "info",
        Level::Warn => "warn",
        Level::Fail => "fail",
    };
    format!("{}/{}/{}", p.label, lv, rs_describe(&p.first))
}

pub fn rs_make_plan(label: &String) -> Plan {
    Plan { label: label.clone(), first: Action::Rename(Box::new(("a".to_string(), "b".to_string()))), level: Level::Warn }
}
