all:
	rm -f *.err obs.txt
	echo '--- py ---' >> obs.txt
	-morloc make -o roots-py roots-py.loc 2>> build.err
	-printf '%s' 'plusH ' >> obs.txt; ./roots-py plusH 10 >> obs.txt 2>> obs.err
	-printf '%s' 'letLambda ' >> obs.txt; ./roots-py letLambda 10 >> obs.txt 2>> obs.err
	-printf '%s' 'composed ' >> obs.txt; ./roots-py composed 10 >> obs.txt 2>> obs.err
	-printf '%s' 'nestedHead ' >> obs.txt; ./roots-py nestedHead 10 >> obs.txt 2>> obs.err
	-printf '%s' 'conditional ' >> obs.txt; ./roots-py conditional 10 >> obs.txt 2>> obs.err
	-printf '%s' 'valuePrefix ' >> obs.txt; ./roots-py valuePrefix 10 >> obs.txt 2>> obs.err
	-morloc make -o shared-py shared-py.loc 2>> build.err
	-printf '%s' 'useShared ' >> obs.txt; ./shared-py useShared 10 >> obs.txt 2>> obs.err
	-printf '%s' 'useSharedComposed ' >> obs.txt; ./shared-py useSharedComposed 10 >> obs.txt 2>> obs.err
	echo '--- cpp ---' >> obs.txt
	-morloc make -o roots-cpp roots-cpp.loc 2>> build.err
	-printf '%s' 'plusH ' >> obs.txt; ./roots-cpp plusH 10 >> obs.txt 2>> obs.err
	-printf '%s' 'letLambda ' >> obs.txt; ./roots-cpp letLambda 10 >> obs.txt 2>> obs.err
	-printf '%s' 'composed ' >> obs.txt; ./roots-cpp composed 10 >> obs.txt 2>> obs.err
	-printf '%s' 'nestedHead ' >> obs.txt; ./roots-cpp nestedHead 10 >> obs.txt 2>> obs.err
	-printf '%s' 'conditional ' >> obs.txt; ./roots-cpp conditional 10 >> obs.txt 2>> obs.err
	-printf '%s' 'valuePrefix ' >> obs.txt; ./roots-cpp valuePrefix 10 >> obs.txt 2>> obs.err
	-morloc make -o shared-cpp shared-cpp.loc 2>> build.err
	-printf '%s' 'useShared ' >> obs.txt; ./shared-cpp useShared 10 >> obs.txt 2>> obs.err
	-printf '%s' 'useSharedComposed ' >> obs.txt; ./shared-cpp useSharedComposed 10 >> obs.txt 2>> obs.err
	echo '--- r ---' >> obs.txt
	-morloc make -o roots-r roots-r.loc 2>> build.err
	-printf '%s' 'plusH ' >> obs.txt; ./roots-r plusH 10 >> obs.txt 2>> obs.err
	-printf '%s' 'letLambda ' >> obs.txt; ./roots-r letLambda 10 >> obs.txt 2>> obs.err
	-printf '%s' 'composed ' >> obs.txt; ./roots-r composed 10 >> obs.txt 2>> obs.err
	-printf '%s' 'nestedHead ' >> obs.txt; ./roots-r nestedHead 10 >> obs.txt 2>> obs.err
	-printf '%s' 'conditional ' >> obs.txt; ./roots-r conditional 10 >> obs.txt 2>> obs.err
	-printf '%s' 'valuePrefix ' >> obs.txt; ./roots-r valuePrefix 10 >> obs.txt 2>> obs.err
	-morloc make -o shared-r shared-r.loc 2>> build.err
	-printf '%s' 'useShared ' >> obs.txt; ./shared-r useShared 10 >> obs.txt 2>> obs.err
	-printf '%s' 'useSharedComposed ' >> obs.txt; ./shared-r useSharedComposed 10 >> obs.txt 2>> obs.err
	echo '--- rust ---' >> obs.txt
	-morloc make -o roots-rust roots-rust.loc 2>> build.err
	-printf '%s' 'plusH ' >> obs.txt; ./roots-rust plusH 10 >> obs.txt 2>> obs.err
	-printf '%s' 'letLambda ' >> obs.txt; ./roots-rust letLambda 10 >> obs.txt 2>> obs.err
	-printf '%s' 'composed ' >> obs.txt; ./roots-rust composed 10 >> obs.txt 2>> obs.err
	-printf '%s' 'nestedHead ' >> obs.txt; ./roots-rust nestedHead 10 >> obs.txt 2>> obs.err
	-printf '%s' 'conditional ' >> obs.txt; ./roots-rust conditional 10 >> obs.txt 2>> obs.err
	-printf '%s' 'valuePrefix ' >> obs.txt; ./roots-rust valuePrefix 10 >> obs.txt 2>> obs.err
	-morloc make -o shared-rust shared-rust.loc 2>> build.err
	-printf '%s' 'useShared ' >> obs.txt; ./shared-rust useShared 10 >> obs.txt 2>> obs.err
	-printf '%s' 'useSharedComposed ' >> obs.txt; ./shared-rust useSharedComposed 10 >> obs.txt 2>> obs.err
	echo '--- nexus ---' >> obs.txt
	-morloc make -o roots-nexus roots-nexus.loc 2>> build.err
	-printf '%s' 'letLambda ' >> obs.txt; ./roots-nexus letLambda 10 >> obs.txt 2>> obs.err
	-printf '%s' 'partialPair ' >> obs.txt; ./roots-nexus partialPair 10 >> obs.txt 2>> obs.err
	echo '--- counts ---' >> obs.txt
	-morloc make -o counts-py counts-py.loc 2>> build.err
	-rm -f tick.log; printf '%s' 'useStaged ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./counts-py useStaged 1 >> obs.txt 2>> obs.err
	-rm -f tick.log; printf '%s' 'ladder ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./counts-py ladder 1 >> obs.txt 2>> obs.err
	-rm -f tick.log; printf '%s' 'fullyApplied ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./counts-py fullyApplied 1 >> obs.txt 2>> obs.err
	-rm -f tick.log; printf '%s' 'recursive ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./counts-py recursive 3 >> obs.txt 2>> obs.err
	-rm -f tick.log; printf '%s' 'nonTail ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./counts-py nonTail 3 >> obs.txt 2>> obs.err
	-rm -f tick.log; printf '%s' 'mutual ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./counts-py mutual 3 >> obs.txt 2>> obs.err
	-rm -f tick.log; printf '%s' 'selfAsValue ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./counts-py selfAsValue 3 >> obs.txt 2>> obs.err
	-rm -f tick.log; printf '%s' 'cycleThroughSelfRecursive ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./counts-py cycleThroughSelfRecursive 3 >> obs.txt 2>> obs.err
	-rm -f tick.log; printf '%s' 'calledFromOutside ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./counts-py calledFromOutside 3 >> obs.txt 2>> obs.err
	-rm -f tick.log; printf '%s' 'partialConstant ' >> obs.txt; TICK_LOG=$(CURDIR)/tick.log ./counts-py partialConstant 3 >> obs.txt 2>> obs.err

clean:
	rm -rf counts-py roots-nexus roots-py roots-cpp roots-r roots-rust shared-py shared-cpp shared-r shared-rust __pycache__ *.err tick.log
	rm -rf *-build *build.tmp*
