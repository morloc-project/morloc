all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err && echo "UNEXPECTED_SUCCESS" > obs.txt || grep -c "must declare both namespace and import" build.err > obs.txt

clean:
	rm -rf nexus pools *.err *-build *build.tmp*
