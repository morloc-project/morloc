all:
	rm -f *.err obs.txt tick.log
	echo '--- py ---' >> obs.txt
	morloc make -o main-py main-py.loc 2>> build.err
	-rm -f tick.log; printf '%s' 'value ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py value 1 >> obs.txt 2>> obs.err
	-rm -f tick.log; printf '%s' 'pickTicks ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py pickTicks 1 >> obs.txt 2>> obs.err
	-rm -f tick.log; printf '%s' 'bumpTicks ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py bumpTicks 1 >> obs.txt 2>> obs.err
	-rm -f tick.log; printf '%s' 'localTicks ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-py localTicks 1 >> obs.txt 2>> obs.err
	echo '--- cpp ---' >> obs.txt
	morloc make -o main-cpp main-cpp.loc 2>> build.err
	-rm -f tick.log; printf '%s' 'value ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp value 1 >> obs.txt 2>> obs.err
	-rm -f tick.log; printf '%s' 'pickTicks ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp pickTicks 1 >> obs.txt 2>> obs.err
	-rm -f tick.log; printf '%s' 'bumpTicks ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp bumpTicks 1 >> obs.txt 2>> obs.err
	-rm -f tick.log; printf '%s' 'localTicks ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cpp localTicks 1 >> obs.txt 2>> obs.err
	echo '--- rust ---' >> obs.txt
	morloc make -o main-rust main-rust.loc 2>> build.err
	-rm -f tick.log; printf '%s' 'value ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust value 1 >> obs.txt 2>> obs.err
	-rm -f tick.log; printf '%s' 'pickTicks ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust pickTicks 1 >> obs.txt 2>> obs.err
	-rm -f tick.log; printf '%s' 'bumpTicks ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust bumpTicks 1 >> obs.txt 2>> obs.err
	-rm -f tick.log; printf '%s' 'localTicks ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-rust localTicks 1 >> obs.txt 2>> obs.err
	echo '--- r ---' >> obs.txt
	morloc make -o main-r main-r.loc 2>> build.err
	-rm -f tick.log; printf '%s' 'value ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r value 1 >> obs.txt 2>> obs.err
	-rm -f tick.log; printf '%s' 'pickTicks ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r pickTicks 1 >> obs.txt 2>> obs.err
	-rm -f tick.log; printf '%s' 'bumpTicks ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r bumpTicks 1 >> obs.txt 2>> obs.err
	-rm -f tick.log; printf '%s' 'localTicks ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-r localTicks 1 >> obs.txt 2>> obs.err
	echo '--- cross ---' >> obs.txt
	morloc make -o main-cross main-cross.loc 2>> build.err
	-rm -f tick.log; printf '%s' 'value ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cross value 1 >> obs.txt 2>> obs.err
	-rm -f tick.log; printf '%s' 'pickTicks ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cross pickTicks 1 >> obs.txt 2>> obs.err
	-rm -f tick.log; printf '%s' 'localTicks ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./main-cross localTicks 1 >> obs.txt 2>> obs.err

clean:
	rm -rf main-py main-cpp main-rust main-r main-cross __pycache__ *.err tick.log
	rm -rf *-build *build.tmp*
