all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus isEven 4 >> obs.txt 2>> obs.err
	./nexus isEven 7 >> obs.txt 2>> obs.err
	./nexus isOdd 4 >> obs.txt 2>> obs.err
	./nexus isOdd 7 >> obs.txt 2>> obs.err
	./nexus isEven 0 >> obs.txt 2>> obs.err
	./nexus ping 5 0 >> obs.txt 2>> obs.err
	./nexus ping 4 0 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
