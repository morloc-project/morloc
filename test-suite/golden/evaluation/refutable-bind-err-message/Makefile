all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus pyBind 1 >> obs.txt 2>> obs.err
	if ./nexus pyBind -- -1 2> py.err ; then echo UNEXPECTED ; elif grep -q '"Err"' py.err || ! grep -q 'negative input' py.err ; then echo UNEXPECTED ; else echo OK ; fi >> obs.txt
	./nexus cppBind 1 >> obs.txt 2>> obs.err
	if ./nexus cppBind -- -1 2> cpp.err ; then echo UNEXPECTED ; elif grep -q '"Err"' cpp.err || ! grep -q 'negative input' cpp.err ; then echo UNEXPECTED ; else echo OK ; fi >> obs.txt

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
