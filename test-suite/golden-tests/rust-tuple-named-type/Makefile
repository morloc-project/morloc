all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus size 3 >> obs.txt 2>> obs.err
	./nexus wrap 3 >> obs.txt 2>> obs.err
	./nexus wrap 200000 2>> obs.err | wc -c >> obs.txt

clean:
	rm -rf nexus pools *.err
	rm -rf *-build *build.tmp*
