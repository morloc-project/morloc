# `$N` naming a streamed `@parse` argument is refused when the program is
# built, with the argument and the action named; `$N` naming another
# argument of the same command builds and runs.
all:
	: > obs.txt
	rm -f *.err
	if morloc make -o nexus-bad bad.loc 2> build-bad.err ; then echo UNEXPECTED >> obs.txt ; else echo OK >> obs.txt ; fi
	grep -o 'refers with `$$1` to a streamed `@parse` argument' build-bad.err >> obs.txt
	morloc make -o nexus-good good.loc 2> build.err
	./nexus-good c:nums.txt x= --tagged >> obs.txt 2>> obs.err

clean:
	rm -rf nexus-bad nexus-good pools __pycache__ *.err
	rm -rf *-build *build.tmp*
