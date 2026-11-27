# Generated ids and source positions are stripped before comparing.
all:
	rm -f *.err obs.txt
	for l in cpp rs; do morloc make -o nexus-$$l main-$$l.loc 2>> build.err; done
	for l in cpp rs; do \
	  fs='"caught 3" "caughtThenFails 3" "threaded 3"'; \
	  if [ $$l = cpp ]; then fs="$$fs \"threadedFails 3\""; fi; \
	  eval "set -- $$fs"; \
	  for f in "$$@"; do \
	    echo "$$l $$f" >> obs.txt; \
	    ./nexus-$$l $$f >> obs.txt 2> run.err || sed -E 's/ \(mid=[0-9]+[^)]*\)//; /^Error: run failed/d' run.err | grep -v "^$$" >> obs.txt; \
	    cat run.err >> obs.err; \
	  done; \
	done

clean:
	rm -rf nexus-* pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
