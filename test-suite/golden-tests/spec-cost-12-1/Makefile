all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus chain 0 >> obs.txt 2>> obs.txt
	./nexus sharedPure 1 >> obs.txt 2>> obs.txt
	./nexus caught 5 >> obs.txt 2>> obs.txt
	./nexus rustChain 0 >> obs.txt 2>> obs.txt
	./nexus loop 2 0 >> obs.txt 2>> obs.txt
	./nexus rustLoop 1 0 >> obs.txt 2>> obs.txt
	./nexus rerun 0 >> obs.txt 2>> obs.txt

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
