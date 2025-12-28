# Pushing to the github registry requires a personal token with package
# permissions. Login is required, for example:
#
#   $ echo $GITHUB_TOKEN | podman login ghcr.io -u morloc-project --password-stdin
#
# I am currently using the "classic" token (ghp_*), not the fine-grained
# token. These need to be refreshed every few months
#
# You need to provide the desired morloc version as an environmental variable,
# so run the make commands like so:
#
# $ make MORLOC_VERSION=0.58.2 build-tiny

define HEREDOC
#!/bin/bash
mkdir -p ~/.morloc
podman run --rm \
  -e HOME=$$HOME \
  -v $$HOME/.morloc:$$HOME/.morloc \
  -v $$PWD:$$HOME \
  -w $$HOME ghcr.io/morloc-project/morloc/morloc-tiny:edge \
  morloc "$$@"
endef
export HEREDOC

install:
	# Pull the exact version to ensure it exists locally
	podman pull ghcr.io/morloc-project/morloc/morloc-tiny:edge
	@mkdir -p ${HOME}/bin
	@echo "$$HEREDOC" > ${HOME}/bin/morloc-edge
	chmod 755 ${HOME}/bin/morloc-edge


# Build a container that just has the morloc compiler
build-tiny:
	podman build --no-cache --force-rm -t ghcr.io/morloc-project/morloc/morloc-tiny:$(MORLOC_VERSION) tiny
	# Also tag the same image as edge locally so edge points to this digest
	podman tag ghcr.io/morloc-project/morloc/morloc-tiny:$(MORLOC_VERSION) ghcr.io/morloc-project/morloc/morloc-tiny:edge

# Build the required docker image
build-full:
	podman build --no-cache --force-rm -t ghcr.io/morloc-project/morloc/morloc-full:$(MORLOC_VERSION) full
	# Also tag the same image as edge locally so edge points to this digest
	podman tag ghcr.io/morloc-project/morloc/morloc-full:$(MORLOC_VERSION) ghcr.io/morloc-project/morloc/morloc-full:edge

# Build the required docker image
build-test:
	podman build --no-cache --force-rm -t ghcr.io/morloc-project/morloc/morloc-test test

shell:
	podman run --shm-size=4g --rm -it ghcr.io/morloc-project/morloc/morloc-full:edge /bin/bash

shell-tiny:
	podman run --shm-size=4g --rm -it ghcr.io/morloc-project/morloc/morloc-tiny:edge /bin/bash

shell-test:
	podman run --shm-size=4g --rm -it ghcr.io/morloc-project/morloc/morloc-test /bin/bash

# push local containers to the github registry
# Ensures edge moves to the same digest as $(MORLOC_VERSION)
push:
	# Push tiny version and edge
	podman push ghcr.io/morloc-project/morloc/morloc-tiny:$(MORLOC_VERSION)
	podman push ghcr.io/morloc-project/morloc/morloc-tiny:edge
	# Push full version and edge
	podman push ghcr.io/morloc-project/morloc/morloc-full:$(MORLOC_VERSION)
	podman push ghcr.io/morloc-project/morloc/morloc-full:edge
	# Push test (no moving tag)
	podman push ghcr.io/morloc-project/morloc/morloc-test

# retrieve the latest morloc builds from the github registry
pull:
	podman pull ghcr.io/morloc-project/morloc/morloc-tiny:$(MORLOC_VERSION)
	podman pull ghcr.io/morloc-project/morloc/morloc-full:$(MORLOC_VERSION)
	podman pull ghcr.io/morloc-project/morloc/morloc-test

# Convenience: pull moving edge tags
pull-edge:
	podman pull ghcr.io/morloc-project/morloc/morloc-tiny:edge
	podman pull ghcr.io/morloc-project/morloc/morloc-full:edge

# Cleanup of podman images may be done as follows
# $ podman image prune
# $ podman container prune
# $ podman image rm -f $(podman image ls -q)
