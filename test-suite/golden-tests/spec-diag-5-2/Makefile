STRIP = sed -e 's/mid=[0-9]*, //g' -e 's/[0-9a-f]\{16\}\.pkt/H.pkt/g'
all:
	rm -rf *.err obs.txt dbg
	morloc make --debug -o nexus main.loc 2> build.err
	for c in "boomP 0" "boomC 0" "boomR 0" "ratio 1 0"; do \
	  echo "== $$c" >> obs.txt; rm -rf dbg; MORLOC_DEBUG_DIR=dbg ./nexus $$c 2>&1 | $(STRIP) >> obs.txt; \
	done
	rm -rf dbg

clean:
	rm -rf nexus pools __pycache__ dbg
	rm -rf *-build *build.tmp*
	rm -f *.err
