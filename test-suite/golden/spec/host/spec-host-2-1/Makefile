all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus runPy '[1,2]' '[10,20]' > obs.txt 2>> obs.err
	./nexus runCpp '[3,4]' '[30,40]' >> obs.txt 2>> obs.err
	./nexus runR '[5,6]' '[50,60]' >> obs.txt 2>> obs.err
	./nexus runRust '[7,8]' '[70,80]' >> obs.txt 2>> obs.err
	cat obs.err >> obs.txt

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
