all:
	rm -f *.err obs.txt
	if morloc make -o nexus main.loc 2> build.err; then echo UNEXPECTED > obs.txt; \
	elif grep -q 'would name both geo-util.Pt and geo_util.Pt' build.err; then echo OK > obs.txt; \
	else echo UNEXPECTED > obs.txt; fi

clean:
	rm -rf nexus pools *err *-build *build.tmp*
