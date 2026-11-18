all:
	rm -f *.err obs.txt *.mpk
	morloc make -o nexus main.loc 2> build.err
	./nexus describeLit 0 >> obs.txt 2>> obs.err
	./nexus describeIn '"C"' '{"Rect":[2.0,3.0]}' >> obs.txt 2>> obs.err
	./nexus viaCpp '[{"Circle":[1.5]},"Empty",{"Rect":[2.0,4.0]}]' >> obs.txt 2>> obs.err
	./nexus depth '{"Rose":[1,[{"Rose":[2,[{"Rose":[3,[]]}]]},{"Rose":[4,[]]}]]}' >> obs.txt 2>> obs.err
	./nexus opt '{"Some":[7]}' >> obs.txt 2>> obs.err
	./nexus opt '"None"' >> obs.txt 2>> obs.err
	./nexus saved s.mpk '[{"Circle":[2.0]},"Empty"]' >> obs.txt 2>> obs.err
	./nexus firstBase '["G","A"]' >> obs.txt 2>> obs.err
	./nexus bad 1 >> obs.txt 2> bad.err || grep -c "Expected a value of the \`data\` type" bad.err >> obs.txt

clean:
	rm -rf nexus pools __pycache__ *.err *.mpk *-build *build.tmp*
