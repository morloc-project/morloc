all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus combine '{"n":1}' '{"n":2}' > obs.txt 2> obs.err
	./nexus empty 5 >> obs.txt 2>> obs.err
	./nexus viaHelper '{"n":3}' '{"n":4}' >> obs.txt 2>> obs.err
	./nexus boxed '{"v":7}' '{"v":8}' >> obs.txt 2>> obs.err
	./nexus nested '{"n":5}' '{"n":6}' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
