all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	# Prose survives verbatim: the `@` is not dropped, no colon is invented,
	# and a bare URL is not read as a directive.
	printf -- '=== help ===\n' >> obs.txt
	./nexus f --help 2>> obs.err | sed -n '1,5p' >> obs.txt
	# A sentence in the legacy `key:` form, and a bare URL, are prose.
	if grep -qE "'Note'|'http'" build.err; then \
	  echo "FAIL: legacy-form prose warned" >> obs.txt; else echo "OK: legacy-form prose is silent" >> obs.txt; fi
	# `@word` is the closed grammar, so an unknown keyword is still reported
	# -- but the line reaches the help as written either way.
	if grep -q "'append'" build.err; then \
	  echo "OK: unknown closed-form keyword warns" >> obs.txt; else echo "FAIL: closed-form silent" >> obs.txt; fi
	# The directive among them still took effect.
	if ./nexus f --alt >/dev/null 2>&1; then \
	  echo "OK: render directive works" >> obs.txt; else echo "FAIL: render directive lost" >> obs.txt; fi
	# A renamed command answers to its new name.
	if ./nexus renamed 1 >/dev/null 2>&1; then \
	  echo "OK: name directive works" >> obs.txt; else echo "FAIL: name directive lost" >> obs.txt; fi
	# A key one edit from a real directive is a typo, and still warns.
	morloc make -o typo typo.loc 2> typo.err || true
	if grep -q "did you mean one of: metavar" typo.err; then \
	  echo "OK: near miss warns" >> obs.txt; else echo "FAIL: near miss silent" >> obs.txt; fi

clean:
	rm -rf nexus typo pools __pycache__ obs.err build.err typo.err
	rm -rf *-build *build.tmp*
