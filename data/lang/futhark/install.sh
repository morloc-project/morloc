#!/usr/bin/env bash
#
# Provision the Futhark compiler into a morloc environment.
#
# Futhark is a hosted guest language (its sourced functions are compiled to C and
# called from the C++ pool); it is NOT on conda-forge, so it cannot be provisioned
# through pixi like the other languages. morloc-manager runs this script at image
# build time inside an OCI container -- the one OS we fully control (debian/ubuntu,
# linux-x86_64). It is deliberately NOT portable to native or other backends;
# those raise an "install script not yet supported" error rather than guessing.
#
# The presence of this file is what marks `futhark` as a script-provisioned
# language (see morloc-deps `layout::SCRIPT_LANGUAGES`). Runs as root at image
# build (apt + install into /usr/local).

set -euo pipefail

FUTHARK_VERSION="0.26.4"
release="futhark-${FUTHARK_VERSION}-linux-x86_64"

# xz-utils to unpack the .tar.xz; make to run the tarball's `install` target (just
# copies the prebuilt binary). curl + ca-certificates come from the image base.
# Self-contained so this works on a minimal release image, not only the dev image.
apt-get update
apt-get install -y --no-install-recommends xz-utils make
curl -sSL "https://futhark-lang.org/releases/${release}.tar.xz" -o "/tmp/${release}.tar.xz"
tar -xf "/tmp/${release}.tar.xz" -C /tmp
make -C "/tmp/${release}" install PREFIX=/usr/local
rm -rf "/tmp/${release}.tar.xz" "/tmp/${release}"
