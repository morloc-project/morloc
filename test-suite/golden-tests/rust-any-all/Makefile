all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus anyNonNeg '[1,-2,3]'  >> obs.txt 2>> obs.err
	./nexus anyNonNeg '[-1,-2]'   >> obs.txt 2>> obs.err
	./nexus allNonNeg '[1,2,3]'   >> obs.txt 2>> obs.err
	./nexus allNonNeg '[1,-2,3]'  >> obs.txt 2>> obs.err
	./nexus anyEven   '[1,3,4]'   >> obs.txt 2>> obs.err
	./nexus anyEven   '[1,3,5]'   >> obs.txt 2>> obs.err
	./nexus allSmall  '[1,5,9]'   >> obs.txt 2>> obs.err
	./nexus allSmall  '[1,5,99]'  >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
