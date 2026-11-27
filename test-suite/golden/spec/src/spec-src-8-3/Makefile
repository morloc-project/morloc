STATE := $(CURDIR)/state
REAL := $${MORLOC_STATE:-$${MORLOC_HOME:-$$HOME/.local/share/morloc}}
M := MORLOC_STATE=$(STATE) morloc
PROG := $(STATE)/exe/prog/prog

all:
	rm -rf obs.txt *.err state
	mkdir -p $(STATE)/src/morloc/plane/std
	cp -r $(REAL)/src/morloc/plane/std/root $(REAL)/src/morloc/plane/std/root-py $(REAL)/src/morloc/plane/std/internal $(STATE)/src/morloc/plane/std/
	cp $(REAL)/LAYOUT $(STATE)/
	mkdir -p $(STATE)/fdb
	cp $(REAL)/fdb/std.root.module $(REAL)/fdb/std.root-py.module $(REAL)/fdb/std.internal.module $(STATE)/fdb/
	printf 'def bump(x):\n    return x + 100.0\n' > bumpmod/bump.py
	printf 'def own(x):\n    return x\n' > prog/own.py
	$(M) install ./bumpmod 2>> build.err
	cd prog && $(M) make --install -f main.loc 2>> ../build.err
	$(PROG) foo 1 > obs.txt 2>> obs.err
	printf 'def own(x):\n    return x + 0.5\n' > prog/own.py
	$(PROG) foo 1 >> obs.txt 2>> obs.err
	printf 'def bump(x):\n    return x + 200.0\n' > bumpmod/bump.py
	$(M) install --force ./bumpmod 2> reinstall.err; echo "reinstall: exit $$?" >> obs.txt
	grep -c "Installed program 'prog' sources files that changed" reinstall.err >> obs.txt || true
	$(PROG) foo 1 >> obs.txt 2>> obs.err
	printf 'def bump(x):\n    return x + 300.0\n' > bumpmod/bump.py
	$(M) install --force --no-rebuild ./bumpmod 2> norebuild.err; echo "no-rebuild: exit $$?" >> obs.txt
	grep -c "it will not run until it is rebuilt" norebuild.err >> obs.txt || true
	$(PROG) foo 1 >> obs.txt 2> refused.err; echo "refused: exit $$?" >> obs.txt
	grep -c "has changed since the program was built" refused.err >> obs.txt || true

clean:
	$(M) uninstall prog
	rm -rf state *.err prog/*-build bumpmod/bump.py prog/own.py
