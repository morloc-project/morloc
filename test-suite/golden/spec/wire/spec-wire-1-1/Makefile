all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus roundTrip '[["b",55],["a",42]]' 2>> obs.err | sed "s/ //g" >> obs.txt

clean:
	rm -rf nexus pools log __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
