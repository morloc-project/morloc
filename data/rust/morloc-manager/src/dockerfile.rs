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

    // The conda env is NOT baked into the image: it is materialized into a
    // host-mounted /env at env setup and bind-mounted at run, so an in-container
    // `morloc make` can mutate it. This only puts that (mounted-at-run) toolchain
    // bin on PATH -- PATH is the load-bearing part of pixi activation, so language
    // runtimes resolve without re-activating.
    out.push_str("# Toolchain PATH (the conda env is mounted at /env at run time)\n");
    out.push_str(&format!(
        "ENV PATH=\"{}:${{PATH}}\"\n",
        crate::serve::CONTAINER_PIXI_ENV_BIN
    ));
    out.push('\n');

    // The morloc runtime shims (libmorloc.so, morloc-nexus, language bindings)
    // are built by `morloc init` into MORLOC_HOME at env-setup (a container step
    // that bind-mounts MORLOC_HOME), NOT baked here -- so they live in a
    // host-mounted, mutable dir, rebuildable if a dependency bumps the core
    // toolchain. This ENV points at that (mounted-at-run) prefix.
    out.push_str("# MORLOC_HOME (shims materialized into it at env setup, mounted at run)\n");
    out.push_str(&format!("ENV MORLOC_HOME={}\n", input.morloc_home));
    out.push('\n');
    out.push_str("WORKDIR /work\n");
    out.push('\n');

    // Self-activate the conda toolchain for EVERY container process -- the
    // interactive shell, `morloc make`, and the cargo/cc-rs it spawns (see
    // serve::conda_activate_lines for why activate.d must be sourced). The pixi
    // path is absolute because the run-time PATH does not include /opt/pixi/bin.
    let activate: String = crate::serve::conda_activate_lines()
        .iter()
        .map(|l| format!(" '{l}'"))
        .collect();
    out.push_str("# Self-activate the conda toolchain for every container process\n");
    out.push_str(&format!(
        "RUN printf '%s\\n' '#!/bin/bash'{activate} 'exec \"$@\"' > /usr/local/bin/morloc-activate && chmod +x /usr/local/bin/morloc-activate\n"
    ));
    out.push_str("ENTRYPOINT [\"/usr/local/bin/morloc-activate\"]\n");

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

# Toolchain PATH (the conda env is mounted at /env at run time)
ENV PATH=\"/env/.pixi/envs/default/bin:${PATH}\"

# MORLOC_HOME (shims materialized into it at env setup, mounted at run)
ENV MORLOC_HOME=/opt/morloc

WORKDIR /work

# Self-activate the conda toolchain for every container process
RUN printf '%s\\n' '#!/bin/bash' 'export CONDA_PREFIX=/env/.pixi/envs/default' 'eval \"$(/opt/pixi/bin/pixi shell-hook --manifest-path /env/pixi.toml --shell bash 2>/dev/null)\" || true' 'for f in \"$CONDA_PREFIX/etc/conda/activate.d/\"*.sh; do [ -r \"$f\" ] && . \"$f\"; done' 'exec \"$@\"' > /usr/local/bin/morloc-activate && chmod +x /usr/local/bin/morloc-activate
ENTRYPOINT [\"/usr/local/bin/morloc-activate\"]
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
