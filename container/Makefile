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
# $ make MORLOC_VERSION=0.54.1 build-tiny

define HEREDOC
#!/bin/bash
mkdir -p ~/.morloc
podman run --rm \
  -e HOME=$$HOME \
  -v $$HOME/.morloc:$$HOME/.morloc \
  -v $$PWD:$$HOME \
  -w $$HOME morloc-tiny:latest \
  morloc "$$@"
endef
export HEREDOC

install:
	podman pull ghcr.io/morloc-project/morloc/morloc-tiny:$(MORLOC_VERSION)
	@echo "$$HEREDOC" > ${HOME}/bin/morloc-$(MORLOC_VERSION)
	chmod 755 ${HOME}/bin/morloc-latest

# Build a container that just has the morloc compiler
build-tiny:
	podman build --no-cache --force-rm -t ghcr.io/morloc-project/morloc/morloc-tiny:$(MORLOC_VERSION) tiny

# Build the required docker image
build-full:
	podman build --no-cache --force-rm -t ghcr.io/morloc-project/morloc/morloc-full:$(MORLOC_VERSION) full

# Build the required docker image
build-test:
	podman build --no-cache --force-rm -t ghcr.io/morloc-project/morloc/morloc-test test

# Open a shell with a nice environment (vim and other niceties)
shell:
	podman run --shm-size=4g --rm -it ghcr.io/morloc-project/morloc/morloc-full:$(MORLOC_VERSION) /bin/bash

shell-tiny:
	podman run --shm-size=4g --rm -it ghcr.io/morloc-project/morloc/morloc-tiny:$(MORLOC_VERSION) /bin/bash

shell-test:
	podman run --shm-size=4g --rm -it ghcr.io/morloc-project/morloc/morloc-test /bin/bash

# push local containers to the github registry
push:
	podman push ghcr.io/morloc-project/morloc/morloc-tiny:$(MORLOC_VERSION)
	podman push ghcr.io/morloc-project/morloc/morloc-full:$(MORLOC_VERSION)
	podman push ghcr.io/morloc-project/morloc/morloc-test

# retrieve the latest morloc builds from the github registry
pull:
	podman pull ghcr.io/morloc-project/morloc/morloc-tiny:$(MORLOC_VERSION)
	podman pull ghcr.io/morloc-project/morloc/morloc-full:$(MORLOC_VERSION)
	podman pull ghcr.io/morloc-project/morloc/morloc-test


# Cleanup of podman images may be done as follows
# $ podman image prune
# $ podman container prune
# $ podman image rm -f $(podman image ls -q)
