all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err && echo "UNEXPECTED_SUCCESS" > obs.txt || grep -c "would name both geo-util.Pt and geo_util.Pt" build.err > obs.txt

clean:
	rm -rf nexus pools __pycache__ *.err *-build *build.tmp*
