all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus argLiteral 5 >> obs.txt 2>> obs.err
	./nexus argParam 5 >> obs.txt 2>> obs.err
	./nexus argLet 5 >> obs.txt 2>> obs.err
	./nexus argEffectful 5 >> obs.txt 2>> obs.err
	./nexus recParam 5 >> obs.txt 2>> obs.err
	./nexus rustArgParam 5 >> obs.txt 2>> obs.err
	./nexus rustRecParam 5 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
