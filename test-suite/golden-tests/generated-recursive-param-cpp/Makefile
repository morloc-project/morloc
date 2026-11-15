all:
	rm -f *.err obs.txt
	if timeout 60s morloc make -o nexus main.loc 2> build.err; then echo UNEXPECTED > obs.txt; \
	elif grep -q 'the record Container refers to itself' build.err; then echo OK > obs.txt; \
	else echo UNEXPECTED > obs.txt; fi

clean:
	rm -rf nexus pools *err *-build *build.tmp*
