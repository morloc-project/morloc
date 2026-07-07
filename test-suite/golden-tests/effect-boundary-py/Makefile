all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus nullaryIo       >  obs.txt 2> obs.err
	./nexus unaryIo         >> obs.txt 2>> obs.err
	./nexus multiEffectIo   >> obs.txt 2>> obs.err
	./nexus pointFree       >> obs.txt 2>> obs.err
	./nexus doWithBind      >> obs.txt 2>> obs.err
	./nexus doBareStmt      >> obs.txt 2>> obs.err
	./nexus callbackLambda  >> obs.txt 2>> obs.err
	./nexus callbackBare    >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err __pycache__
