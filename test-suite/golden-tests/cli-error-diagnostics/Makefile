all:
	rm -f *.err
	: > obs.txt
	morloc make -o progA progA.loc 2> build.err
	if morloc make -o rnexus reserved.loc 2>>build.err; then echo "FAIL: mlcp_ prefix accepted" >> obs.txt; else grep -qF 'reserved `mlcp_` prefix' build.err && echo "OK: mlcp_ reserved" >> obs.txt || echo "FAIL: wrong error text" >> obs.txt; fi
	if morloc-nexus run @ ./progA 5 2>&1 | grep -qE 'required|missing'; then echo "OK: @-before-target rejected" >> obs.txt; else echo "FAIL: @-before-target accepted" >> obs.txt; fi

clean:
	rm -rf progA rnexus pools __pycache__ *.err
