all:
	rm -f *.err
	: > obs.txt
	morloc make -o progA progA.loc 2> build.err
	if morloc make -o rnexus reserved.loc 2>>build.err; then echo "FAIL: mlcp_ prefix accepted" >> obs.txt; else grep -qF 'reserved `mlcp_` prefix' build.err && echo "OK: mlcp_ reserved" >> obs.txt || echo "FAIL: wrong error text" >> obs.txt; fi
	if morloc-nexus run @ ./progA 5 2>&1 | grep -qE 'required|missing'; then echo "OK: @-before-target rejected" >> obs.txt; else echo "FAIL: @-before-target accepted" >> obs.txt; fi
	if morloc make -o knexus keycollide.loc 2>>build.err; then echo "FAIL: colliding argument key accepted" >> obs.txt; else grep -qF 'is the interface name of' build.err && echo "OK: colliding argument key rejected" >> obs.txt || echo "FAIL: wrong error text" >> obs.txt; fi

clean:
	rm -rf progA rnexus knexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
