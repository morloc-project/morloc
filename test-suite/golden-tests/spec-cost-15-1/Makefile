RUN = MORLOC_CACHE_BASE=$$PWD/cache ./nexus

all:
	rm -rf *.err obs.txt cache
	mkdir cache
	morloc make -o nexus main.loc 2> build.err
	$(RUN) letTwo 1 >> obs.txt 2>> obs.err
	$(RUN) letThree 1 >> obs.txt 2>> obs.err
	$(RUN) chain 1 >> obs.txt 2>> obs.err
	$(RUN) shadowPy 10 >> obs.txt 2>> obs.err
	$(RUN) letShadow 1 >> obs.txt 2>> obs.err
	$(RUN) letRecord 1 >> obs.txt 2> ticks.err
	grep -c tick ticks.err >> obs.txt
	$(RUN) chosenOnce 2 >> obs.txt 2> ticks.err
	grep -c tick ticks.err >> obs.txt
	$(RUN) chosenOnce 0 >> obs.txt 2>> obs.err
	$(RUN) tryCopy 1 >> obs.txt 2>> obs.err
	$(RUN) labeled 1 >> obs.txt 2>> obs.err
	$(RUN) labeled 1 >> obs.txt 2>> obs.err
	$(RUN) recCopy 1 >> obs.txt 2>> obs.err
	$(RUN) hostHome 1 >> obs.txt 2>> obs.err
	$(RUN) hostDirect 1 >> obs.txt 2>> obs.err
	if morloc typecheck -v --realize main.loc > check.err 2> /dev/null && ! grep -q 'cost check' check.err; then echo "OK: built placement costs the least" >> obs.txt; else echo "UNEXPECTED: built placement does not cost the least found" >> obs.txt; fi
	morloc make -o bad bad-host.loc 2> bad-host.err || true
	if grep -q "cannot leave the pool that made it" bad-host.err; then echo "OK: a C++ callback does not call a host function" >> obs.txt; else echo "UNEXPECTED: bad-host.loc was not refused" >> obs.txt; fi

clean:
	rm -rf nexus bad pools __pycache__ cache
	rm -rf *-build *build.tmp*
	rm -f *.err
