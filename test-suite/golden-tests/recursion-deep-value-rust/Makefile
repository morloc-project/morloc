all:
	rm -f *.err
	: > obs.txt
	morloc make -X rust:lto=thin -o nexus main.loc 2> build.err
	./nexus chainCount 10 >> obs.txt 2>> obs.err
	./nexus chainCount 30000 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
