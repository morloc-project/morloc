//! Generate a Dockerfile that builds a morloc environment image from the SAME
//! requirement->pixi lowering the native backend uses.
//!
//! The image is requirement-derived, not hand-authored: a slim base, a pinned
//! pixi, the environment's `pixi.toml`/`pixi.lock`, the pinned morloc runtime
//! trio, and `morloc init` run against the pixi toolchain. Container and native
//! thus share one source of truth (the pixi manifest); they differ only in
//! isolation. Build-extras (OS packages conda cannot provide) are the one
//! container-only input -- native has no build layer.

/// Build-time extras that conda cannot provide. `system_packages` are installed
/// with the base image's package manager (assumed Debian-family `apt`). Extra
/// conda channels are NOT here -- they belong in the `pixi.toml` the manifest
/// renderer produces.
#[derive(Debug, Clone, Default)]
pub struct BuildExtras {
    pub system_packages: Vec<String>,
}

/// Inputs to the Dockerfile generator. The `pixi.toml`, `pixi.lock`, and the
/// `runtime/` trio are supplied through the build context (COPY-ed in), not
/// here; this struct carries only the image-shape parameters.
pub struct DockerfileInput<'a> {
    /// Slim base image (glibc + a Debian-family package manager),
    /// e.g. "debian:bookworm-slim".
    pub base_image: &'a str,
    /// Pinned pixi version without a leading 'v', e.g. "0.76.2".
    pub pixi_version: &'a str,
    /// In-image MORLOC_HOME (where `morloc init` installs the shims).
    pub morloc_home: &'a str,
    /// Build-extras (container-only OS packages).
    pub extras: &'a BuildExtras,
}

/// Render the Dockerfile text. Deterministic for a given input.
pub fn generate_dockerfile(input: &DockerfileInput) -> String {
    let mut out = String::new();
    out.push_str(&format!("FROM {}\n", input.base_image));
    out.push_str("ENV DEBIAN_FRONTEND=noninteractive\n");
    out.push('\n');

    // Base tools always needed (curl for the pixi installer, CA certs for TLS),
    // plus any container-only system packages conda cannot provide.
    out.push_str("# Base tools + build-extras (system packages conda cannot provide)\n");
    out.push_str("RUN apt-get update \\\n");
    out.push_str(" && apt-get install -y --no-install-recommends ca-certificates curl");
    for pkg in &input.extras.system_packages {
        out.push_str(&format!(" {pkg}"));
    }
    out.push_str(" \\\n");
    out.push_str(" && rm -rf /var/lib/apt/lists/*\n");
    out.push('\n');

    // Pinned pixi (the conda package manager).
    out.push_str("# Pinned pixi (conda package manager)\n");
    out.push_str("ENV PIXI_HOME=/opt/pixi\n");
    out.push_str("ENV PATH=\"/opt/pixi/bin:${PATH}\"\n");
    out.push_str(&format!(
        "RUN curl -fsSL https://pixi.sh/install.sh | PIXI_VERSION=v{} bash\n",
        input.pixi_version
    ));
    out.push('\n');

    // Pinned, version-coherent morloc runtime trio (libmorloc.so + morloc-nexus
    // + the morloc compiler + rust sources), supplied via the build context.
    out.push_str("# Pinned morloc runtime trio (from the build context)\n");
    out.push_str("COPY runtime/ /opt/morloc-runtime/\n");
    out.push_str("ENV MORLOC_RUST_BIN=/opt/morloc-runtime\n");
    out.push_str("ENV PATH=\"/opt/morloc-runtime:${PATH}\"\n");
    out.push('\n');

    // The environment's conda world, reproduced from the lock.
    out.push_str("# Environment requirements (reproduced from the lock)\n");
    out.push_str("WORKDIR /env\n");
    out.push_str("COPY pixi.toml pixi.lock ./\n");
    out.push_str("RUN pixi install --locked\n");
    out.push('\n');

    // Build morloc's language shims against the pixi toolchain.
    out.push_str("# Build morloc's language shims against the pixi toolchain\n");
    out.push_str(&format!("ENV MORLOC_HOME={}\n", input.morloc_home));
    out.push_str("RUN pixi run morloc init -f\n");
    out.push('\n');
    out.push_str("WORKDIR /work\n");

    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generates_expected_dockerfile() {
        let extras = BuildExtras {
            system_packages: vec!["libgl1".to_string(), "graphviz".to_string()],
        };
        let input = DockerfileInput {
            base_image: "debian:bookworm-slim",
            pixi_version: "0.76.2",
            morloc_home: "/opt/morloc",
            extras: &extras,
        };
        let got = generate_dockerfile(&input);
        let expected = "\
FROM debian:bookworm-slim
ENV DEBIAN_FRONTEND=noninteractive

# Base tools + build-extras (system packages conda cannot provide)
RUN apt-get update \\
 && apt-get install -y --no-install-recommends ca-certificates curl libgl1 graphviz \\
 && rm -rf /var/lib/apt/lists/*

# Pinned pixi (conda package manager)
ENV PIXI_HOME=/opt/pixi
ENV PATH=\"/opt/pixi/bin:${PATH}\"
RUN curl -fsSL https://pixi.sh/install.sh | PIXI_VERSION=v0.76.2 bash

# Pinned morloc runtime trio (from the build context)
COPY runtime/ /opt/morloc-runtime/
ENV MORLOC_RUST_BIN=/opt/morloc-runtime
ENV PATH=\"/opt/morloc-runtime:${PATH}\"

# Environment requirements (reproduced from the lock)
WORKDIR /env
COPY pixi.toml pixi.lock ./
RUN pixi install --locked

# Build morloc's language shims against the pixi toolchain
ENV MORLOC_HOME=/opt/morloc
RUN pixi run morloc init -f

WORKDIR /work
";
        assert_eq!(got, expected);
    }

    #[test]
    fn no_extra_packages_still_installs_base_tools() {
        let extras = BuildExtras::default();
        let input = DockerfileInput {
            base_image: "debian:bookworm-slim",
            pixi_version: "0.76.2",
            morloc_home: "/opt/morloc",
            extras: &extras,
        };
        let got = generate_dockerfile(&input);
        assert!(got.contains("ca-certificates curl \\"));
        assert!(!got.contains("  \\")); // no dangling double space before continuation
    }
}
