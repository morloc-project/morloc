all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus optInt null >> obs.txt 2>> obs.err
	./nexus optInt 3 >> obs.txt 2>> obs.err
	./nexus optStr a >> obs.txt 2>> obs.err
	./nexus pairInt 5 >> obs.txt 2>> obs.err
	./nexus pairStr b >> obs.txt 2>> obs.err
	./nexus nested null >> obs.txt 2>> obs.err
	./nexus aliased 4 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err
	rm -rf *-build *build.tmp*
