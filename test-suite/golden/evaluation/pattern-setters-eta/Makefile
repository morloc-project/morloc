all:
	rm -f *.err obs.txt
	morloc make -o nexus-py main-py.loc 2> build.err
	morloc make -o nexus-r main-r.loc 2>> build.err
	morloc make -o nexus-cpp main-cpp.loc 2>> build.err
	echo "--- nexus-py ---" >> obs.txt
	./nexus-py etaSingle >> obs.txt 2>> obs.err
	./nexus-py etaTwo >> obs.txt 2>> obs.err
	./nexus-py etaReversed >> obs.txt 2>> obs.err
	./nexus-py etaCaptured 9.5 >> obs.txt 2>> obs.err
	./nexus-py etaCapturedTwo 9.5 7.5 >> obs.txt 2>> obs.err
	./nexus-py etaNested 9.5 >> obs.txt 2>> obs.err
	./nexus-py etaTuple >> obs.txt 2>> obs.err
	./nexus-py etaTupleCaptured 9 >> obs.txt 2>> obs.err
	./nexus-py etaGetter >> obs.txt 2>> obs.err
	./nexus-py etaGetterGroup >> obs.txt 2>> obs.err
	./nexus-py etaOfEta >> obs.txt 2>> obs.err
	echo "--- nexus-r ---" >> obs.txt
	./nexus-r etaSingle >> obs.txt 2>> obs.err
	./nexus-r etaTwo >> obs.txt 2>> obs.err
	./nexus-r etaReversed >> obs.txt 2>> obs.err
	./nexus-r etaCaptured 9.5 >> obs.txt 2>> obs.err
	./nexus-r etaCapturedTwo 9.5 7.5 >> obs.txt 2>> obs.err
	./nexus-r etaNested 9.5 >> obs.txt 2>> obs.err
	./nexus-r etaTuple >> obs.txt 2>> obs.err
	./nexus-r etaTupleCaptured 9 >> obs.txt 2>> obs.err
	./nexus-r etaGetter >> obs.txt 2>> obs.err
	./nexus-r etaGetterGroup >> obs.txt 2>> obs.err
	./nexus-r etaOfEta >> obs.txt 2>> obs.err
	echo "--- nexus-cpp ---" >> obs.txt
	./nexus-cpp etaSingle >> obs.txt 2>> obs.err
	./nexus-cpp etaTwo >> obs.txt 2>> obs.err
	./nexus-cpp etaReversed >> obs.txt 2>> obs.err
	./nexus-cpp etaCaptured 9.5 >> obs.txt 2>> obs.err
	./nexus-cpp etaCapturedTwo 9.5 7.5 >> obs.txt 2>> obs.err
	./nexus-cpp etaNested 9.5 >> obs.txt 2>> obs.err
	./nexus-cpp etaTuple >> obs.txt 2>> obs.err
	./nexus-cpp etaTupleCaptured 9 >> obs.txt 2>> obs.err
	./nexus-cpp etaGetter >> obs.txt 2>> obs.err
	./nexus-cpp etaGetterGroup >> obs.txt 2>> obs.err
	./nexus-cpp etaOfEta >> obs.txt 2>> obs.err

clean:
	rm -rf nexus-py nexus-r nexus-cpp pools __pycache__
	rm -f *.err
	rm -rf *-build *build.tmp*
