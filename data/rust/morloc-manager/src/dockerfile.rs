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
    /// Script-provisioned languages (e.g. futhark) whose `install.sh` the builder
    /// has written into the build context as `install-<lang>.sh`. Each is COPY-ed
    /// in and run at image build. OCI-only (only the Dockerfile path supports it).
    pub lang_installs: &'a [String],
    /// Dev environment: the compiler + Rust source are NOT baked in (they are
    /// built from a mounted source tree at materialize time), so the
    /// `COPY runtime/` step and the baked `MORLOC_RUST_DIR` are omitted;
    /// `CONTAINER_RUNTIME_BIN` becomes a mount target instead.
    pub dev: bool,
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

    // Script-provisioned languages (e.g. futhark): their upstream binary is not on
    // conda-forge, so run the committed install.sh (staged into the build context)
    // against the base OS. Runs as root at build; the script does its own apt.
    for lang in input.lang_installs {
        out.push_str(&format!("# {lang}: provisioned by data/lang/{lang}/install.sh\n"));
        out.push_str(&format!("COPY install-{lang}.sh /tmp/morloc-install-{lang}.sh\n"));
        out.push_str(&format!(
            "RUN bash /tmp/morloc-install-{lang}.sh && rm /tmp/morloc-install-{lang}.sh\n\n"
        ));
    }

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
    //
    // Dev envs skip the COPY + baked MORLOC_RUST_DIR: the compiler is BUILT from a
    // mounted source tree at materialize time and installed into CONTAINER_RUNTIME_BIN
    // (a mount, not a baked layer), and MORLOC_RUST_DIR points at the mounted
    // source. Only the PATH entry is kept, so the built compiler is found.
    let runtime_bin = crate::serve::CONTAINER_RUNTIME_BIN;
    if input.dev {
        out.push_str("# morloc compiler + rust source are built from a mounted source tree\n");
    } else {
        out.push_str("# morloc compiler + rust source (from the build context)\n");
        out.push_str(&format!("COPY runtime/ {runtime_bin}/\n"));
        out.push_str(&format!("ENV MORLOC_RUST_DIR={runtime_bin}/rust\n"));
    }
    // Both variants put the compiler dir on PATH (baked COPY dest, or a mount).
    out.push_str(&format!("ENV PATH=\"{runtime_bin}:${{PATH}}\"\n"));
    out.push('\n');

    if input.dev {
        // Bake the Haskell toolchain (ghcup + stack) into the dev image, so
        // `stack`/`ghc` are on PATH in an interactive dev shell -- a dev env is a
        // place to build/edit/rebuild the compiler, not just run a prebuilt one
        // (mirrors the project's reference dev container). GHC itself is NOT baked:
        // `stack setup` fetches the exact version stack.yaml pins into
        // `$HOME/.stack` (host-mounted), so it persists and stays authoritative.
        // MINIMAL installs ghcup only; the second step adds a current stack.
        let ghcup_bin = crate::serve::CONTAINER_GHCUP_BIN;
        out.push_str("# Haskell toolchain (ghcup + stack) to build the compiler from source\n");
        out.push_str("ENV GHCUP_INSTALL_BASE_PREFIX=/opt BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1\n");
        out.push_str("RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh \\\n");
        out.push_str(&format!("  && {ghcup_bin}/ghcup install stack --set\n"));
        out.push_str(&format!("ENV PATH=\"{ghcup_bin}:${{PATH}}\"\n"));
        out.push('\n');
    }

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
            lang_installs: &[],
            dev: false,
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
            lang_installs: &[],
            dev: false,
        };
        let got = generate_dockerfile(&input);
        assert!(got.contains("ca-certificates curl \\"));
        assert!(!got.contains("  \\")); // no dangling double space before continuation
    }

    #[test]
    fn lang_install_scripts_are_copied_and_run() {
        let extras = BuildExtras::default();
        let installs = vec!["futhark".to_string()];
        let input = DockerfileInput {
            base_image: "debian:bookworm-slim",
            pixi_version: "0.76.2",
            morloc_home: "/opt/morloc",
            extras: &extras,
            lang_installs: &installs,
            dev: false,
        };
        let got = generate_dockerfile(&input);
        assert!(got.contains("COPY install-futhark.sh /tmp/morloc-install-futhark.sh"));
        assert!(got.contains("RUN bash /tmp/morloc-install-futhark.sh"));
        // Installed against the base OS, before the pixi install.
        let install_at = got.find("morloc-install-futhark.sh").unwrap();
        let pixi_at = got.find("pixi.sh/install.sh").unwrap();
        assert!(install_at < pixi_at);
    }

    #[test]
    fn dev_dockerfile_omits_copy_runtime() {
        let extras = BuildExtras::default();
        let input = DockerfileInput {
            base_image: "debian:bookworm-slim",
            pixi_version: "0.76.2",
            morloc_home: "/opt/morloc",
            extras: &extras,
            lang_installs: &[],
            dev: true,
        };
        let got = generate_dockerfile(&input);
        // Nothing is baked in: the compiler is built from a mounted source tree.
        assert!(!got.contains("COPY runtime/"));
        assert!(!got.contains("ENV MORLOC_RUST_DIR="));
        // But CONTAINER_RUNTIME_BIN (a mount target) is still on PATH.
        assert!(got.contains(&format!("ENV PATH=\"{}:", crate::serve::CONTAINER_RUNTIME_BIN)));
    }
}
