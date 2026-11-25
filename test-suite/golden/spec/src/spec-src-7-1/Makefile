MAKE_NEXUS = morloc make -o nexus main.loc

all:
	rm -rf *.err obs.txt __pycache__ inc.py
	printf 'def inc(x)\n    return x + 1.0\n' > inc.py
	if $(MAKE_NEXUS) 2> build.err; then echo "unchecked: built" > obs.txt; else echo "unchecked: failed" > obs.txt; fi
	if $(MAKE_NEXUS) -X py:check=on 2> check.err; then echo "checked: built" >> obs.txt; else echo "checked: failed" >> obs.txt; fi
	grep -c 'SyntaxError' check.err >> obs.txt || true
	if $(MAKE_NEXUS) -X py:check=maybe 2> value.err; then echo "bad value: built" >> obs.txt; else echo "bad value: failed" >> obs.txt; fi
	grep -c 'expected one of on, off' value.err >> obs.txt || true
	printf 'def inc(x):\n    return x + 1.0\n' > inc.py
	if $(MAKE_NEXUS) -X py:check=on 2>> build.err; then echo "fixed: built" >> obs.txt; else echo "fixed: failed" >> obs.txt; fi
	ls __pycache__ 2>/dev/null | grep -c '^inc\.' >> obs.txt || true
	./nexus inc 1 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *-build *build.tmp* __pycache__ inc.py
