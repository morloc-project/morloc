all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	for c in foldCpp foldRust ; do echo "--- $$c ---" >> obs.txt ; ./nexus $$c --n >> obs.txt 2>> obs.err ; done ; true
	for c in forkCpp forkRust ; do echo "--- $$c ---" >> obs.txt ; if ./nexus $$c --n >> obs.txt 2> fold.err ; then echo UNEXPECTED >> obs.txt ; else echo OK >> obs.txt ; fi ; grep -c "no wire form" fold.err >> obs.txt ; cat fold.err >> obs.err ; done

clean:
	rm -rf nexus pools *.err
	rm -rf *-build *build.tmp*
