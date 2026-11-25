all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus travel -h 2>> obs.err | grep -c "Gust speed, if measured" >> obs.txt || true
	./nexus travel -g 4 2>> obs.err >> obs.txt

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
