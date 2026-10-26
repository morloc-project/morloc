all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus localOk 5 >> obs.txt 2>> obs.err
	morloc make -o bad bad.loc 2> bad.err || true
	if grep -q "would be passed into cpp" bad.err; then echo "OK refused at build time" >> obs.txt; else echo "UNEXPECTED: a host-created closure was not refused" >> obs.txt; fi

clean:
	rm -rf nexus bad __pycache__ *.err
	rm -rf *-build *build.tmp*
