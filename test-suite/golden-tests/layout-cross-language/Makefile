all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus optInts '[1,null,-3,null,5]' >> obs.txt 2>> obs.err
	./nexus boolPairs '[[true,null],[false,true],[true,false]]' >> obs.txt 2>> obs.err
	./nexus smalls '[{"a":1,"b":2},{"a":255,"b":0}]' >> obs.txt 2>> obs.err
	./nexus dirs '["South","North","East"]' >> obs.txt 2>> obs.err
	./nexus shapes '[{"A":[true]},{"B":[null]},{"B":[false]}]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err __pycache__
	rm -rf *-build *build.tmp*
