all:
	rm -f *.err obs.txt
	morloc typecheck --realize main.loc 2> build.err | sed -n '/^placement:$$/,/^$$/p' > obs.txt
	morloc make -o nexus main.loc 2>> build.err
	./nexus doubled 3 >> obs.txt
	./nexus quadrupled 3 >> obs.txt
	morloc make -o bad bad.loc 2> bad.err || true
	if grep -q "Ambiguous source declarations for 'twice'" bad.err; then echo "OK: tied sources refused" >> obs.txt; else echo "UNEXPECTED: tied sources accepted" >> obs.txt; fi

clean:
	rm -rf nexus bad pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
