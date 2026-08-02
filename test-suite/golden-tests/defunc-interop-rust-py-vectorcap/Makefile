all:
	: > obs.txt
	rm -f *.err
	morloc make --inline-size 0 -o nexus main.loc 2> build.err
	./nexus crossApply '[1,2,3]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
