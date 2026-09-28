RUN = MORLOC_INLINE_SIZE=0

all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	for C in "held 50" "heldPy 50" "partial 5 40" "partialPy 5 40" "partialRust 5 40" "alias 50" "aliasRust 50"; do \
	  echo "$$C: $$($(RUN) ./nexus $$C 2>> obs.err || echo failed)" >> obs.txt; \
	done

clean:
	rm -rf nexus pools *.err __pycache__
	rm -rf *-build *build.tmp*
