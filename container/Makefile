# # Pushing to the github registry requires a personal token with package
# # permsions. Login is required as required:
#
# $ echo $GITHUB_TOKEN | podman login ghcr.io -u morloc-project --password-stdin

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
	podman pull ghcr.io/morloc-project/morloc/morloc-tiny:latest
	@echo "$$HEREDOC" > ${HOME}/bin/morloc-latest
	chmod 755 ${HOME}/bin/morloc-latest

# Build a container that just has the morloc compiler
build-tiny:
	podman build -t ghcr.io/morloc-project/morloc/morloc-tiny:latest tiny

# Build the required docker image
build-full:
	podman build -t ghcr.io/morloc-project/morloc/morloc-full:latest full

# Open a shell with a nice environment (vim and other niceties)
shell:
	podman run -w "/home/morloc" -it morloc-full /bin/sh

# push local containers to the github registry
push:
	podman push ghcr.io/morloc-project/morloc/morloc-tiny:latest
	podman push ghcr.io/morloc-project/morloc/morloc-full:latest

# retrieve the latest morloc builds from the github registry
pull:
	podman pull ghcr.io/morloc-project/morloc/morloc-tiny:latest
	podman pull ghcr.io/morloc-project/morloc/morloc-full:latest


# Cleanup of podman images may be done as follows
# $ podman image prune
# $ podman container prune
# $ podman image rm -f $(podman image ls -q)
