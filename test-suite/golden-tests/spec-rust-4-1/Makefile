all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus deep 1000 > obs.txt 2> obs.err
	./nexus viaPy 7 >> obs.txt 2>> obs.err
	./nexus built 3.5 >> obs.txt 2>> obs.err
	./nexus evalRs '{"Add":[{"Num":[1.5]},{"Maybe1":[{"Neg":[{"Num":[0.5]}]}]}]}' >> obs.txt 2>> obs.err
	./nexus roundTrip '{"Shared":[{"Maybe1":[null]},"t"]}' >> obs.txt 2>> obs.err
	./nexus negOf '{"Neg":[{"Num":[4]}]}' >> obs.txt 2>> obs.err
	./nexus addRight '{"Add":[{"Num":[1]},{"Num":[2]}]}' >> obs.txt 2>> obs.err
	./nexus maybeOf '{"Maybe1":[{"Num":[5]}]}' >> obs.txt 2>> obs.err
	./nexus --keep-null maybeOf '{"Maybe1":[null]}' >> obs.txt 2>> obs.err
	./nexus sharedOf '{"Shared":[{"Num":[6]},"u"]}' >> obs.txt 2>> obs.err
	./nexus sharedTag '{"Shared":[{"Num":[6]},"u"]}' >> obs.txt 2>> obs.err
	./nexus chainDepth 1000 >> obs.txt 2>> obs.err
	if timeout 300s morloc make -o refuse-old refuse-old.loc 2> build-refuse-old.err; then echo UNEXPECTED >> obs.txt; \
	elif grep -q 'user-written Rust enum arm' build-refuse-old.err; then echo 'refused old' >> obs.txt; \
	else echo UNEXPECTED >> obs.txt; fi
	if timeout 300s morloc make -o refuse-width refuse-width.loc 2> build-refuse-width.err; then echo UNEXPECTED >> obs.txt; \
	elif grep -q 'user-written Rust enum arm' build-refuse-width.err; then echo 'refused width' >> obs.txt; \
	else echo UNEXPECTED >> obs.txt; fi

clean:
	rm -rf nexus refuse-old refuse-width pools __pycache__ *err *-build *build.tmp*
