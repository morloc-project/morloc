STRIP = sed -e 's/mid=[0-9]*, //g' -e 's/(pid [0-9]*)//'
all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus deep 2 2>&1 | $(STRIP) > obs.txt
	-./nexus guarded 0 >> obs.txt 2>> obs.err
	-./nexus guarded 3 >> obs.txt 2>> obs.err
	./nexus caught 2 2>&1 | $(STRIP) >> obs.txt

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
