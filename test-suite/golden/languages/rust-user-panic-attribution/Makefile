all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus tryDiv 7 0 >> obs.txt 2> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus tryHelper 3 >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus tryDiv 7 2 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
