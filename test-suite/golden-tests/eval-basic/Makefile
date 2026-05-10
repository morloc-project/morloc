all:
	rm -f obs.txt obs.err case5.err
	printf 'case1: ' >> obs.txt
	morloc eval "import root-py; 1+2" >> obs.txt 2>> obs.err
	printf 'case2: ' >> obs.txt
	morloc eval "$$(printf 'import root-py\n1+2')" >> obs.txt 2>> obs.err
	printf 'case3: ' >> obs.txt
	morloc eval "import root-py; import root-cpp; 1+2" >> obs.txt 2>> obs.err
	printf 'case4: ' >> obs.txt
	morloc eval 'import root-py; "foo" <> "bar"' >> obs.txt 2>> obs.err
	printf 'case5_exit: ' >> obs.txt
	if morloc eval "1+2" >/dev/null 2>case5.err; then \
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
	morloc eval 'import root-py; let { a = 10; b = 20 } in a + b' >> obs.txt 2>> obs.err
	printf 'case7: ' >> obs.txt
	morloc eval 'import root-py; a + b where { a = 10; b = 20 }' >> obs.txt 2>> obs.err

clean:
	rm -f obs.err case5.err
