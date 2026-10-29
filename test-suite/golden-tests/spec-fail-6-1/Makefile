all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	for f in viaCpp viaRust tryCpp guardCpp guardPy guardR; do \
		./nexus $$f 1 >> obs.txt 2>> obs.err; echo "$$f: exit $$?" >> obs.txt; \
	done
	printf 'internal errors: %s\n' $$(grep -c 'internal error' obs.err) >> obs.txt

clean:
	rm -rf nexus pools log __pycache__ *.err
	rm -rf *-build *build.tmp*
