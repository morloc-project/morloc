all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus mixed       > obs.txt 2> obs.err
	./nexus sameTyped   >> obs.txt 2>> obs.err
	./nexus reversed    >> obs.txt 2>> obs.err
	./nexus nested      >> obs.txt 2>> obs.err
	./nexus tupled      >> obs.txt 2>> obs.err
	./nexus argApplied  >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
