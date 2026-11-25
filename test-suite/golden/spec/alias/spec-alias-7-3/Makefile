all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus topPy 1 2>> obs.err >> obs.txt
	./nexus topCpp 1 2>> obs.err >> obs.txt
	./nexus roundPy '{"R":[null,[{"R":[null,[]]}]]}' 2>> obs.err >> obs.txt
	./nexus roundCpp '{"R":[null,[{"R":[null,[]]}]]}' 2>> obs.err >> obs.txt

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
