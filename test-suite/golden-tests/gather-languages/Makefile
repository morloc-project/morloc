all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	for c in rowsCpp rowsPy rowsR rowsRust strsCpp strsPy strsR strsRust ; do for h in cpp py r rust ; do echo "--- $$c --$$h ---" >> obs.txt ; ./nexus $$c 4 --$$h >> obs.txt 2>> obs.err ; done ; done ; true
	for c in hitsCpp hitsPy hitsRust ; do for h in cpp py rust ; do echo "--- $$c --$$h ---" >> obs.txt ; ./nexus $$c 3 --$$h >> obs.txt 2>> obs.err ; done ; done ; true

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
