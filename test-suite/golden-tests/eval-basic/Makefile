MHOME := $${MORLOC_HOME:-$$HOME/.local/share/morloc}

all:
	rm -f obs.txt obs.err case5.err
	@# Pre-clean any leftover --save artifacts so the test is rerunnable.
	@for n in eval-basic-foo eval-basic-bar; do \
	  rm -f $(MHOME)/bin/$$n $(MHOME)/fdb/$$n.manifest $(MHOME)/fdb/$$n.eval-meta; \
	  rm -rf $(MHOME)/exe/$$n; \
	done
	printf 'case1: ' >> obs.txt
	morloc eval -e "import root-py; 1+2" >> obs.txt 2>> obs.err
	printf 'case2: ' >> obs.txt
	morloc eval -e "$$(printf 'import root-py\n1+2')" >> obs.txt 2>> obs.err
	printf 'case3: ' >> obs.txt
	morloc eval -e "import root-py; import root-cpp; 1+2" >> obs.txt 2>> obs.err
	printf 'case4: ' >> obs.txt
	morloc eval -e 'import root-py; "foo" <> "bar"' >> obs.txt 2>> obs.err
	printf 'case5_exit: ' >> obs.txt
	if morloc eval -e "1+2" >/dev/null 2>case5.err; then \
	  printf 'PASS\n' >> obs.txt; \
	else \
	  printf 'FAIL\n' >> obs.txt; \
	fi
	printf 'case5_msg: ' >> obs.txt
	if grep -q 'Undefined term: +' case5.err; then \
	  printf 'OK\n' >> obs.txt; \
	else \
	  printf 'MISSING\n' >> obs.txt; \
	fi
	printf 'case6: ' >> obs.txt
	morloc eval -e 'import root-py; let { a = 10; b = 20 } in a + b' >> obs.txt 2>> obs.err
	printf 'case7: ' >> obs.txt
	morloc eval -e 'import root-py; a + b where { a = 10; b = 20 }' >> obs.txt 2>> obs.err
	@# -e and a script file are interchangeable: the same expression in
	@# a file must give the same result as case1.
	printf 'case_file: ' >> obs.txt
	morloc eval expr.loc >> obs.txt 2>> obs.err
	@# --save round-trip: two saves with different NAMEs must not collide.
	@# Pre-fix, both saved to exe/main and the second invocation failed.
	morloc eval --save eval-basic-foo -e 'import root-py; 40 + 2' >> obs.err 2>&1
	morloc eval --save eval-basic-bar -e 'import root-py; 100 + 1' >> obs.err 2>&1
	printf 'case8: ' >> obs.txt
	$(MHOME)/bin/eval-basic-foo >> obs.txt 2>> obs.err
	printf 'case9: ' >> obs.txt
	$(MHOME)/bin/eval-basic-bar >> obs.txt 2>> obs.err

clean:
	rm -f obs.err case5.err
	@for n in eval-basic-foo eval-basic-bar; do \
	  rm -f $(MHOME)/bin/$$n $(MHOME)/fdb/$$n.manifest $(MHOME)/fdb/$$n.eval-meta; \
	  rm -rf $(MHOME)/exe/$$n; \
	done
