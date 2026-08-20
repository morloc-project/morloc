//! Version-constraint parsing + intersection for `--lang` pins and the
//! language-support table.
//!
//! `pixi.rs::merge_constraint` only CONCATENATES comma-atoms; it never intersects
//! or detects an empty result, so a bad `--lang` pin dies as a raw conda solver
//! trace three layers down. This module resolves a language's effective version
//! constraint by INTERSECTING the three sources -- program EnvSpec, user pin, and
//! morloc-supported range -- into one interval, detecting an empty intersection
//! and reporting it in morloc's vocabulary at pin time (before pixi is invoked).
//!
//! It handles the conda match-spec SUBSET morloc uses: comma-separated
//! comparator atoms (`>=`, `>`, `<`, `<=`, `==`/bare), `X.Y.*` globs, and `*`
//! (any). Versions are dotted numeric segments compared segment-wise.

use crate::error::{DepsError, Result};
use std::cmp::Ordering;

fn bad(msg: String) -> DepsError {
    DepsError::Env(msg)
}

/// A dotted numeric version (`3.12` -> `[3,12]`), compared segment-wise with
/// zero-padding so `3.9 < 3.10` and `3.12 == 3.12.0`.
#[derive(Clone, Debug)]
struct Version(Vec<u64>);

// Eq/PartialEq are defined via Ord (below) so trailing-zero-equal versions like
// `3.12` and `3.12.0` compare equal, consistent with ordering.
impl PartialEq for Version {
    fn eq(&self, other: &Version) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}
impl Eq for Version {}

impl Version {
    fn parse(s: &str) -> Result<Version> {
        let s = s.trim();
        if s.is_empty() {
            return Err(bad("empty version".into()));
        }
        let segs = s
            .split('.')
            .map(|p| {
                p.parse::<u64>()
                    .map_err(|_| bad(format!("unsupported version segment '{p}' in '{s}'")))
            })
            .collect::<Result<Vec<u64>>>()?;
        Ok(Version(segs))
    }

    /// The next version at the given segment depth: `bump([3,12], 1) -> [3,13]`,
    /// `bump([3], 0) -> [4]`. Used to turn `X.Y.*`/pins into half-open bounds.
    fn bump(&self, depth: usize) -> Version {
        let mut segs = self.0[..=depth.min(self.0.len().saturating_sub(1))].to_vec();
        if let Some(last) = segs.last_mut() {
            *last += 1;
        }
        Version(segs)
    }

    fn render(&self) -> String {
        self.0
            .iter()
            .map(u64::to_string)
            .collect::<Vec<_>>()
            .join(".")
    }
}

impl Ord for Version {
    fn cmp(&self, other: &Version) -> Ordering {
        let n = self.0.len().max(other.0.len());
        for i in 0..n {
            let a = self.0.get(i).copied().unwrap_or(0);
            let b = other.0.get(i).copied().unwrap_or(0);
            match a.cmp(&b) {
                Ordering::Equal => continue,
                ord => return ord,
            }
        }
        Ordering::Equal
    }
}
impl PartialOrd for Version {
    fn partial_cmp(&self, other: &Version) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// The lower bound of a version range.
#[derive(Clone, Debug)]
enum Lower {
    Inf,
    /// `>=v`
    Ge(Version),
    /// `>v`
    Gt(Version),
}

/// The upper bound of a version range.
#[derive(Clone, Debug)]
enum Upper {
    Inf,
    /// `<=v`
    Le(Version),
    /// `<v`
    Lt(Version),
}

/// The effective interval of a constraint (the intersection of its atoms).
#[derive(Clone, Debug)]
pub struct VersionRange {
    lower: Lower,
    upper: Upper,
}

impl VersionRange {
    /// The unbounded range (`*`).
    pub fn any() -> VersionRange {
        VersionRange {
            lower: Lower::Inf,
            upper: Upper::Inf,
        }
    }

    /// Parse a conda match-spec constraint: `*`, `>=3.10,<3.14`, `3.12.*`, or a
    /// bare `3.12` (treated as an exact-prefix glob, i.e. `3.12.*`).
    pub fn parse(spec: &str) -> Result<VersionRange> {
        let spec = spec.trim();
        if spec.is_empty() || spec == "*" {
            return Ok(VersionRange::any());
        }
        let mut range = VersionRange::any();
        for atom in spec.split(',') {
            range = range.intersect(&VersionRange::parse_atom(atom.trim())?);
        }
        Ok(range)
    }

    /// A user pin. `py@3.12` (bare) -> the minor range `>=3.12,<3.13`; an
    /// explicit comparator/glob is parsed as-is; a fully-qualified `3.12.4` pins
    /// that patch prefix (`>=3.12.4,<3.12.5`).
    pub fn from_pin(pin: &str) -> Result<VersionRange> {
        let pin = pin.trim();
        if pin.starts_with(['>', '<', '=']) || pin.contains('*') {
            return VersionRange::parse(pin);
        }
        // Reject syntax from other ecosystems leaking into a conda spec.
        if pin.starts_with('^') || pin.starts_with('~') {
            return Err(bad(format!(
                "unsupported version pin '{pin}': use conda syntax (e.g. 3.12, >=3.12,<3.14), \
                 not cargo '^' or PEP440 '~=' operators"
            )));
        }
        VersionRange::parse_atom(&format!("{pin}.*"))
    }

    /// Parse a single atom: `>=v`, `>v`, `<=v`, `<v`, `==v`, `v.*`, or bare `v`.
    fn parse_atom(atom: &str) -> Result<VersionRange> {
        let atom = atom.trim();
        if let Some(rest) = atom.strip_prefix(">=") {
            return Ok(VersionRange {
                lower: Lower::Ge(Version::parse(rest)?),
                upper: Upper::Inf,
            });
        }
        if let Some(rest) = atom.strip_prefix("<=") {
            return Ok(VersionRange {
                lower: Lower::Inf,
                upper: Upper::Le(Version::parse(rest)?),
            });
        }
        if let Some(rest) = atom.strip_prefix('>') {
            return Ok(VersionRange {
                lower: Lower::Gt(Version::parse(rest)?),
                upper: Upper::Inf,
            });
        }
        if let Some(rest) = atom.strip_prefix('<') {
            return Ok(VersionRange {
                lower: Lower::Inf,
                upper: Upper::Lt(Version::parse(rest)?),
            });
        }
        // `X.Y.*` glob or `==X.Y` or bare `X.Y` -> the prefix range [X.Y, X.(Y+1)).
        let base = atom.strip_prefix("==").unwrap_or(atom);
        let base = base.strip_suffix(".*").unwrap_or(base);
        let v = Version::parse(base)?;
        let depth = v.0.len().saturating_sub(1);
        Ok(VersionRange {
            lower: Lower::Ge(v.clone()),
            upper: Upper::Lt(v.bump(depth)),
        })
    }

    /// Intersect two ranges: the tighter of each bound.
    pub fn intersect(&self, other: &VersionRange) -> VersionRange {
        VersionRange {
            lower: tighter_lower(&self.lower, &other.lower),
            upper: tighter_upper(&self.upper, &other.upper),
        }
    }

    /// True if the range admits no version (lower bound is at or above upper).
    pub fn is_empty(&self) -> bool {
        let (lo, lo_incl) = match &self.lower {
            Lower::Inf => return false, // unbounded-below intersect anything nonempty above
            Lower::Ge(v) => (v, true),
            Lower::Gt(v) => (v, false),
        };
        let (hi, hi_incl) = match &self.upper {
            Upper::Inf => return false,
            Upper::Le(v) => (v, true),
            Upper::Lt(v) => (v, false),
        };
        match lo.cmp(hi) {
            Ordering::Greater => true,
            Ordering::Less => false,
            // lo == hi: nonempty only if BOTH bounds include it (an exact point).
            Ordering::Equal => !(lo_incl && hi_incl),
        }
    }

    /// Render back to a conda match-spec (`*`, `>=3.10,<3.14`, `>=3.12,<3.13`).
    pub fn to_spec(&self) -> String {
        let mut atoms = Vec::new();
        match &self.lower {
            Lower::Inf => {}
            Lower::Ge(v) => atoms.push(format!(">={}", v.render())),
            Lower::Gt(v) => atoms.push(format!(">{}", v.render())),
        }
        match &self.upper {
            Upper::Inf => {}
            Upper::Le(v) => atoms.push(format!("<={}", v.render())),
            Upper::Lt(v) => atoms.push(format!("<{}", v.render())),
        }
        if atoms.is_empty() {
            "*".to_string()
        } else {
            atoms.join(",")
        }
    }
}

fn tighter_lower(a: &Lower, b: &Lower) -> Lower {
    match (a, b) {
        (Lower::Inf, x) | (x, Lower::Inf) => x.clone(),
        _ => {
            let (av, ai) = lower_parts(a);
            let (bv, bi) = lower_parts(b);
            match av.cmp(bv) {
                Ordering::Greater => a.clone(),
                Ordering::Less => b.clone(),
                // same value: exclusive (`>`) is tighter than inclusive (`>=`).
                Ordering::Equal => {
                    if !ai {
                        a.clone()
                    } else if !bi {
                        b.clone()
                    } else {
                        a.clone()
                    }
                }
            }
        }
    }
}

fn tighter_upper(a: &Upper, b: &Upper) -> Upper {
    match (a, b) {
        (Upper::Inf, x) | (x, Upper::Inf) => x.clone(),
        _ => {
            let (av, ai) = upper_parts(a);
            let (bv, bi) = upper_parts(b);
            match av.cmp(bv) {
                Ordering::Less => a.clone(),
                Ordering::Greater => b.clone(),
                Ordering::Equal => {
                    if !ai {
                        a.clone()
                    } else if !bi {
                        b.clone()
                    } else {
                        a.clone()
                    }
                }
            }
        }
    }
}

fn lower_parts(l: &Lower) -> (&Version, bool) {
    match l {
        Lower::Ge(v) => (v, true),
        Lower::Gt(v) => (v, false),
        Lower::Inf => unreachable!(),
    }
}
fn upper_parts(u: &Upper) -> (&Version, bool) {
    match u {
        Upper::Le(v) => (v, true),
        Upper::Lt(v) => (v, false),
        Upper::Inf => unreachable!(),
    }
}

/// Resolve a language's effective version constraint by intersecting the three
/// sources, erroring legibly (in morloc's vocabulary, at the command typed) when
/// they cannot all be satisfied. `program` = the constraint the installed
/// programs need (from EnvSpec); `pin` = the user's `--lang` pin; `supported` =
/// morloc's supported range from the language-support table. Returns the
/// effective conda match-spec.
pub fn resolve_lang_version(
    lang: &str,
    morloc_version: &str,
    program: Option<&str>,
    pin: Option<&str>,
    supported: &str,
) -> Result<String> {
    let mut range = VersionRange::parse(supported)?;
    let mut sources: Vec<(&str, String)> = vec![("morloc supports", supported.to_string())];

    if let Some(p) = program.filter(|s| !s.trim().is_empty() && s.trim() != "*") {
        range = range.intersect(&VersionRange::parse(p)?);
        sources.push(("a program needs", p.to_string()));
    }
    if let Some(p) = pin.filter(|s| !s.trim().is_empty()) {
        let pr = VersionRange::from_pin(p)?;
        range = range.intersect(&pr);
        sources.push(("you pinned", pr.to_spec()));
    }

    if range.is_empty() {
        let detail = sources
            .iter()
            .map(|(who, what)| format!("  {who}: {what}"))
            .collect::<Vec<_>>()
            .join("\n");
        return Err(bad(format!(
            "no {lang} version satisfies all constraints (morloc {morloc_version}):\n{detail}\n\
             The requirements do not overlap. Loosen the pin or use a different backend."
        )));
    }
    Ok(range.to_spec())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn spec(s: &str) -> String {
        VersionRange::parse(s).unwrap().to_spec()
    }

    #[test]
    fn parses_and_renders() {
        assert_eq!(spec("*"), "*");
        assert_eq!(spec(">=3.10,<3.14"), ">=3.10,<3.14");
        assert_eq!(spec("3.12.*"), ">=3.12,<3.13");
        assert_eq!(spec("3.12"), ">=3.12,<3.13"); // bare = prefix glob
    }

    #[test]
    fn pin_semantics() {
        // minor pin
        assert_eq!(VersionRange::from_pin("3.12").unwrap().to_spec(), ">=3.12,<3.13");
        // exact patch pin
        assert_eq!(VersionRange::from_pin("3.12.4").unwrap().to_spec(), ">=3.12.4,<3.12.5");
        // explicit range passes through
        assert_eq!(VersionRange::from_pin(">=3.11").unwrap().to_spec(), ">=3.11");
    }

    #[test]
    fn rejects_foreign_syntax() {
        assert!(VersionRange::from_pin("^1.2").is_err()); // cargo
        assert!(VersionRange::from_pin("~=3.12").is_err()); // PEP440
    }

    #[test]
    fn version_ordering() {
        assert!(Version::parse("3.9").unwrap() < Version::parse("3.10").unwrap());
        assert_eq!(Version::parse("3.12").unwrap(), Version::parse("3.12.0").unwrap());
    }

    #[test]
    fn intersection_clamps() {
        // author >=3.11 intersected with supported >=3.10,<3.14 -> >=3.11,<3.14
        let r = VersionRange::parse(">=3.10,<3.14")
            .unwrap()
            .intersect(&VersionRange::parse(">=3.11").unwrap());
        assert_eq!(r.to_spec(), ">=3.11,<3.14");
        assert!(!r.is_empty());
    }

    #[test]
    fn empty_detection() {
        // program needs >=3.13 but pin is 3.12.* -> empty
        let r = VersionRange::parse(">=3.13")
            .unwrap()
            .intersect(&VersionRange::parse("3.12.*").unwrap());
        assert!(r.is_empty());
        // exact point [3.12,3.12] is nonempty
        let point = VersionRange::parse(">=3.12").unwrap().intersect(&VersionRange::parse("<=3.12").unwrap());
        assert!(!point.is_empty());
        // touching half-open [3.12,3.12) is empty
        let touch = VersionRange::parse(">=3.12").unwrap().intersect(&VersionRange::parse("<3.12").unwrap());
        assert!(touch.is_empty());
    }

    #[test]
    fn resolve_ok_and_err() {
        // ok: default py, supported range, no pin/program
        let got = resolve_lang_version("python", "0.99.0", None, None, ">=3.10,<3.14").unwrap();
        assert_eq!(got, ">=3.10,<3.14");
        // ok: pin within range
        let got = resolve_lang_version("python", "0.99.0", None, Some("3.12"), ">=3.10,<3.14").unwrap();
        assert_eq!(got, ">=3.12,<3.13");
        // err: pin above the supported ceiling
        let err = resolve_lang_version("python", "0.99.0", None, Some("3.14"), ">=3.10,<3.14")
            .unwrap_err()
            .to_string();
        assert!(err.contains("no python version"), "got: {err}");
        assert!(err.contains("you pinned"), "got: {err}");
        assert!(err.contains("morloc supports"), "got: {err}");
        // err: program needs newer than supported
        assert!(resolve_lang_version("python", "0.99.0", Some(">=3.14"), None, ">=3.10,<3.14").is_err());
    }
}
