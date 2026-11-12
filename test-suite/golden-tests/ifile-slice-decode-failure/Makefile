all:
	rm -f *.err obs.txt *.mlc
	morloc make -o nexus main.loc 2> build.err
	./nexus writeBig big.mlc > /dev/null 2>> obs.err
	./nexus readCpp big.mlc >> obs.txt 2>> obs.err
	./nexus readRust big.mlc >> obs.txt 2>> obs.err
	rm -f *.mlc

clean:
	rm -rf nexus pools __pycache__ *.err *.mlc
	rm -rf *-build *build.tmp*
