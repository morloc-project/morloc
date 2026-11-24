all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err && echo "UNEXPECTED_SUCCESS" > obs.txt || grep -c "has a namespace but no import header" build.err > obs.txt

clean:
	rm -rf nexus pools *.err *-build *build.tmp*
