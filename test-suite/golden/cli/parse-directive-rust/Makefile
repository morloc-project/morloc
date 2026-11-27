all:
	rm -f *.err
	: > obs.txt
	cp n.txt n.ptxt
	morloc make -o nexus main.loc 2> build.err
	./nexus total a.csv >> obs.txt 2>> obs.err
	./nexus total '[4,5]' >> obs.txt 2>> obs.err
	./nexus total bad.csv 2>> obs.txt || echo "exit $$?" >> obs.txt
	./nexus sizes n.ptxt >> obs.txt 2>> obs.err
	./nexus sizes n.txt >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err n.ptxt
	rm -rf *-build *build.tmp*
