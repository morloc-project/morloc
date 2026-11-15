all:
	rm -f *.err obs.txt
	if morloc make -o nexus main.loc 2> build.err; then echo UNEXPECTED > obs.txt; \
	elif grep -q 'py has no record kind `dictt`; its kinds are dict' build.err; then echo OK > obs.txt; \
	else echo UNEXPECTED > obs.txt; fi

clean:
	rm -rf nexus pools *err *-build *build.tmp*
