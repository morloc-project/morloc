all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	printf -- '--- effProd (100,110,120; bug=100,100,100) ---\n' >> obs.txt
	./nexus effProd >> obs.txt 2>> obs.err
	printf -- '--- twiceProd (100,430; bug=100,100) ---\n' >> obs.txt
	./nexus twiceProd >> obs.txt 2>> obs.err
	printf -- '--- pureMap ---\n' >> obs.txt
	./nexus pureMap >> obs.txt 2>> obs.err
	printf -- '--- pureTwice ---\n' >> obs.txt
	./nexus pureTwice >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
