SHORT = 12
LONG = 48
CMDS = viaCpp viaPy viaR viaRust rustHosted

# A chunk-at-a-time run holds a chunk or two, so quadrupling its length
# must not change what it needs at once. Comparing a run against a longer
# one of its own asserts that without pinning an allocator's numbers.
all:
	rm -f *.err obs.txt
	morloc make -X rust:lto=thin -o nexus main.loc 2> build.err
	for c in $(CMDS); do \
	  short=$$(./nexus $$c $(SHORT) 2>> obs.err); \
	  long=$$(./nexus $$c $(LONG) 2>> obs.err); \
	  echo "$$c holds-steady=$$([ "$$short" = "$$long" ] && echo yes || echo "no ($$short vs $$long MiB)")" >> obs.txt; \
	done

clean:
	rm -rf nexus pools *.err __pycache__ .RData
	rm -rf *-build *build.tmp*
