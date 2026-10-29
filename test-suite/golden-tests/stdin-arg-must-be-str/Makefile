all:
	rm -f *.err obs.txt
	if morloc make -o nexus main.loc 2> build.err; then echo "FAIL: @stdin on ?Str accepted" > obs.txt; else grep -qF '`@stdin` is only for a `Str` argument' build.err && echo "OK: @stdin on ?Str refused" > obs.txt || echo "FAIL: wrong error text" > obs.txt; fi

clean:
	rm -rf nexus pools *-build *build.tmp* *.err
