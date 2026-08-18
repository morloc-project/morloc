//! Generate a Dockerfile that builds a morloc environment image from the SAME
//! requirement->pixi lowering the native backend uses.
//!
//! The image is requirement-derived, not hand-authored: a slim base, a pinned
//! pixi, the environment's `pixi.toml`/`pixi.lock`, the pinned morloc compiler +
//! Rust source, and `morloc init` run against the pixi toolchain (which builds
//! libmorloc.so + morloc-nexus from that source). Container and native thus
//! share one source of truth (the pixi manifest); they differ only in
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
/// `runtime/` directory (morloc compiler + Rust source) are supplied through the
/// build context (COPY-ed in), not here; this struct carries only the
/// image-shape parameters.
pub struct DockerfileInput<'a> {
    /// Slim base image (glibc + a Debian-family package manager), fully
    /// qualified so podman needs no short-name registry config,
    /// e.g. "docker.io/library/debian:bookworm-slim".
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

    // Base tools needed to bootstrap the image before the conda env exists: curl
    // for the pixi installer, CA certs for TLS. Everything else (compilers, git,
    // language runtimes) comes from the pixi-solved conda env, so the base stays
    // minimal. Plus any container-only system packages conda cannot provide.
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

    // The prebuilt morloc COMPILER + the Rust SOURCE, supplied via the build
    // context. `morloc init` (below) builds libmorloc.so + morloc-nexus from that
    // source with the pixi toolchain, so the runtime is ABI-coherent with the
    // pools. The COPY destination is shared with the run-side PATH
    // (serve::container_path), so it comes from one constant, not a literal.
    let runtime_bin = crate::serve::CONTAINER_RUNTIME_BIN;
    out.push_str("# morloc compiler + rust source (from the build context)\n");
    out.push_str(&format!("COPY runtime/ {runtime_bin}/\n"));
    out.push_str(&format!("ENV MORLOC_RUST_DIR={runtime_bin}/rust\n"));
    out.push_str(&format!("ENV PATH=\"{runtime_bin}:${{PATH}}\"\n"));
    out.push('\n');

    // The environment's conda world, reproduced from the lock. Bake the solved
    // env's bin onto PATH so downstream layers and run/serve inherit the language
    // toolchain (python, R, ...) without re-activating: PATH is the load-bearing
    // part of pixi activation. `default` is pixi's environment name for a
    // manifest with no explicit [environments] table.
    out.push_str("# Environment requirements (reproduced from the lock)\n");
    out.push_str("WORKDIR /env\n");
    out.push_str("COPY pixi.toml pixi.lock ./\n");
    out.push_str("RUN pixi install --locked\n");
    out.push_str(&format!(
        "ENV PATH=\"{}:${{PATH}}\"\n",
        crate::serve::CONTAINER_PIXI_ENV_BIN
    ));
    out.push('\n');

    // Build morloc's language shims against the pixi toolchain. MORLOC_HOME is
    // the immutable runtime prefix (bin/lib/include); `morloc init` builds
    // libmorloc.so + morloc-nexus + the shims here in a proper bin/lib layout.
    // At run time this prefix is
    // never bind-mounted, so the nexus resolves libmorloc.so via its own baked
    // `bin/../lib` RUNPATH -- no LD_LIBRARY_PATH override needed. Mutable state
    // (exe/fdb/modules) is written under MORLOC_STATE, a separate mounted root.
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

# morloc compiler + rust source (from the build context)
COPY runtime/ /opt/morloc-runtime/
ENV MORLOC_RUST_DIR=/opt/morloc-runtime/rust
ENV PATH=\"/opt/morloc-runtime:${PATH}\"

# Environment requirements (reproduced from the lock)
WORKDIR /env
COPY pixi.toml pixi.lock ./
RUN pixi install --locked
ENV PATH=\"/env/.pixi/envs/default/bin:${PATH}\"

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
