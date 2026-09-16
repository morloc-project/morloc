MHOME := $${MORLOC_HOME:-$$HOME/.local/share/morloc}
HOOK := MORLOC_ENV=golden MORLOC_BUILD_HOOK=$(CURDIR)/hook.sh HOOK_LOG=$(CURDIR)/hook.log

all:
	rm -f obs.txt hook.log
	echo "--- make ---" >> obs.txt
	$(HOOK) morloc make -o hookprog hookprog.loc 2>> build.err
	./hookprog add 1 2 >> obs.txt 2>> obs.err
	echo "--- make --install ---" >> obs.txt
	$(HOOK) morloc make --install -f hookprog.loc 2>> build.err
	$(MHOME)/bin/hookprog add 3 4 >> obs.txt 2>> obs.err
	echo "--- hook.log ---" >> obs.txt
	cat hook.log >> obs.txt

clean:
	rm -f hookprog hook.log *.err
	morloc uninstall hookprog
	rm -rf *-build *build.tmp*
