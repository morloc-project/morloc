TMP := $(CURDIR)/gather-tmp

all:
	: > obs.txt
	rm -f *.err
	rm -rf $(TMP)
	mkdir -p $(TMP)
	morloc make -o nexus main.loc 2> build.err
	for c in routeCpp routeRust forksCpp routePy realsPy intsPy pairsPy hitsPy forksPy strsPy ; do echo "--- $$c ---" >> obs.txt ; MORLOC_TMPDIR=$(TMP) ./nexus $$c --show >> obs.txt 2>> obs.err ; done ; true
	for c in foldCpp foldPy foldR foldRust ; do echo "--- $$c ---" >> obs.txt ; ./nexus $$c --n >> obs.txt 2>> obs.err ; done ; true
	echo "--- mapsCpp ---" >> obs.txt ; ./nexus mapsCpp --show >> obs.txt 2>> obs.err ; true
	for c in mapsFoldCpp mapsFoldPy ; do echo "--- $$c ---" >> obs.txt ; if ./nexus $$c --n >> obs.txt 2> fold.err ; then echo UNEXPECTED >> obs.txt ; else echo OK >> obs.txt ; fi ; grep -c "no wire form" fold.err >> obs.txt ; cat fold.err >> obs.err ; done
	find $(TMP) -name "morloc-gather-*" | wc -l | tr -d " " >> obs.txt

clean:
	rm -rf nexus pools __pycache__ $(TMP) *.err
	rm -rf *-build *build.tmp*
