all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus eqLit b >> obs.txt 2>> obs.err
	./nexus eqLit c >> obs.txt 2>> obs.err
	./nexus eqVars a a >> obs.txt 2>> obs.err
	./nexus eqVars a b >> obs.txt 2>> obs.err
	./nexus cmpAll a >> obs.txt 2>> obs.err
	./nexus countStr '["a","b","b"]' >> obs.txt 2>> obs.err
	./nexus countInt '[1,2,1,3]' >> obs.txt 2>> obs.err
	./nexus eqField b >> obs.txt 2>> obs.err
	./nexus allTrue '[true,false,true]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err
	rm -rf *-build *build.tmp*
