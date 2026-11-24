all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err || true
	if grep -q "instance Indexable (Vec 3) selected here implements .__access_index__. by a morloc body" build.err; then echo "OK: refused" >> obs.txt; else echo "UNEXPECTED: not refused" >> obs.txt; fi
	morloc make -o ok ok.loc 2>> ok.err
	./ok two '[1,2]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus ok pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
