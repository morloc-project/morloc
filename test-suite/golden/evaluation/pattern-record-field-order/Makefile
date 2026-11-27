all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus setterThenKey           >  obs.txt 2>  obs.err
	./nexus setterThenGroup         >> obs.txt 2>> obs.err
	./nexus setterThenGroupReversed >> obs.txt 2>> obs.err
	./nexus setterThenOtherKey      >> obs.txt 2>> obs.err
	./nexus setterThenAllOfThree    >> obs.txt 2>> obs.err
	./nexus setterOnBoundRecord     >> obs.txt 2>> obs.err
	./nexus setterAlone             >> obs.txt 2>> obs.err
	./nexus literalThenGroup        >> obs.txt 2>> obs.err
	./nexus tupleSlotThenGroup      >> obs.txt 2>> obs.err
	./nexus bracketThenGroup        >> obs.txt 2>> obs.err
	./nexus letBoundThenGroup       >> obs.txt 2>> obs.err
	./nexus lambdaThenGroup         >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
	rm -f *.err
	rm -rf *-build *build.tmp*
