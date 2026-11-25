all:
	rm -f *.err obs.txt
	morloc make -o nexus-py main-py.loc 2>> build.err
	morloc make -o nexus-cpp main-cpp.loc 2>> build.err
	morloc make -o nexus-r main-r.loc 2>> build.err
	morloc make -o nexus-rust main-rust.loc 2>> build.err
	echo "--- nexus-py ---" >> obs.txt
	./nexus-py guardArm 2 >> obs.txt 2>> obs.err
	./nexus-py matchArm 5 >> obs.txt 2>> obs.err
	./nexus-py boxField 3 >> obs.txt 2>> obs.err
	./nexus-py mapPartial 2 >> obs.txt 2>> obs.err
	./nexus-py guardArm -- -1 2> run.err ; echo "exit:$$?" >> obs.txt
	grep -oE "NEG-GUARD" run.err >> obs.txt || echo NONE >> obs.txt
	./nexus-py mapPartial -- -1 2> run.err ; echo "exit:$$?" >> obs.txt
	grep -oE "NEG-GUARD" run.err >> obs.txt || echo NONE >> obs.txt
	./nexus-py argsFirst -- -1 2> run.err ; echo "exit:$$?" >> obs.txt
	grep -oE "ARG-FAILED|NEG-GUARD" run.err >> obs.txt || echo NONE >> obs.txt
	./nexus-py throwHead 1 2> run.err ; echo "exit:$$?" >> obs.txt
	grep -oE "HEAD-THROWN" run.err >> obs.txt || echo NONE >> obs.txt
	./nexus-py throwHeadArgsFirst 1 2> run.err ; echo "exit:$$?" >> obs.txt
	grep -oE "ARG-FAILED|HEAD-THROWN" run.err >> obs.txt || echo NONE >> obs.txt
	echo "--- nexus-cpp ---" >> obs.txt
	./nexus-cpp guardArm 2 >> obs.txt 2>> obs.err
	./nexus-cpp matchArm 5 >> obs.txt 2>> obs.err
	./nexus-cpp boxField 3 >> obs.txt 2>> obs.err
	./nexus-cpp mapPartial 2 >> obs.txt 2>> obs.err
	./nexus-cpp guardArm -- -1 2> run.err ; echo "exit:$$?" >> obs.txt
	grep -oE "NEG-GUARD" run.err >> obs.txt || echo NONE >> obs.txt
	./nexus-cpp mapPartial -- -1 2> run.err ; echo "exit:$$?" >> obs.txt
	grep -oE "NEG-GUARD" run.err >> obs.txt || echo NONE >> obs.txt
	./nexus-cpp argsFirst -- -1 2> run.err ; echo "exit:$$?" >> obs.txt
	grep -oE "ARG-FAILED|NEG-GUARD" run.err >> obs.txt || echo NONE >> obs.txt
	./nexus-cpp throwHead 1 2> run.err ; echo "exit:$$?" >> obs.txt
	grep -oE "HEAD-THROWN" run.err >> obs.txt || echo NONE >> obs.txt
	./nexus-cpp throwHeadArgsFirst 1 2> run.err ; echo "exit:$$?" >> obs.txt
	grep -oE "ARG-FAILED|HEAD-THROWN" run.err >> obs.txt || echo NONE >> obs.txt
	echo "--- nexus-r ---" >> obs.txt
	./nexus-r guardArm 2 >> obs.txt 2>> obs.err
	./nexus-r matchArm 5 >> obs.txt 2>> obs.err
	./nexus-r boxField 3 >> obs.txt 2>> obs.err
	./nexus-r mapPartial 2 >> obs.txt 2>> obs.err
	./nexus-r guardArm -- -1 2> run.err ; echo "exit:$$?" >> obs.txt
	grep -oE "NEG-GUARD" run.err >> obs.txt || echo NONE >> obs.txt
	./nexus-r mapPartial -- -1 2> run.err ; echo "exit:$$?" >> obs.txt
	grep -oE "NEG-GUARD" run.err >> obs.txt || echo NONE >> obs.txt
	./nexus-r argsFirst -- -1 2> run.err ; echo "exit:$$?" >> obs.txt
	grep -oE "ARG-FAILED|NEG-GUARD" run.err >> obs.txt || echo NONE >> obs.txt
	./nexus-r throwHead 1 2> run.err ; echo "exit:$$?" >> obs.txt
	grep -oE "HEAD-THROWN" run.err >> obs.txt || echo NONE >> obs.txt
	./nexus-r throwHeadArgsFirst 1 2> run.err ; echo "exit:$$?" >> obs.txt
	grep -oE "ARG-FAILED|HEAD-THROWN" run.err >> obs.txt || echo NONE >> obs.txt
	echo "--- nexus-rust ---" >> obs.txt
	./nexus-rust guardArm 2 >> obs.txt 2>> obs.err
	./nexus-rust matchArm 5 >> obs.txt 2>> obs.err
	./nexus-rust boxField 3 >> obs.txt 2>> obs.err
	./nexus-rust mapPartial 2 >> obs.txt 2>> obs.err
	./nexus-rust guardArm -- -1 2> run.err ; echo "exit:$$?" >> obs.txt
	grep -oE "NEG-GUARD" run.err >> obs.txt || echo NONE >> obs.txt
	./nexus-rust mapPartial -- -1 2> run.err ; echo "exit:$$?" >> obs.txt
	grep -oE "NEG-GUARD" run.err >> obs.txt || echo NONE >> obs.txt
	./nexus-rust argsFirst -- -1 2> run.err ; echo "exit:$$?" >> obs.txt
	grep -oE "ARG-FAILED|NEG-GUARD" run.err >> obs.txt || echo NONE >> obs.txt
	./nexus-rust throwHead 1 2> run.err ; echo "exit:$$?" >> obs.txt
	grep -oE "HEAD-THROWN" run.err >> obs.txt || echo NONE >> obs.txt
	./nexus-rust throwHeadArgsFirst 1 2> run.err ; echo "exit:$$?" >> obs.txt
	grep -oE "ARG-FAILED|HEAD-THROWN" run.err >> obs.txt || echo NONE >> obs.txt

clean:
	rm -rf nexus-py nexus-cpp nexus-r nexus-rust pools __pycache__
	rm -f *.err
	rm -rf *-build *build.tmp*
