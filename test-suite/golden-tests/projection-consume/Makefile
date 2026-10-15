all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus onceRust 5 >> obs.txt 2>> obs.err
	./nexus twiceRust 5 >> obs.txt 2>> obs.err
	./nexus pairRust 5 >> obs.txt 2>> obs.err
	./nexus loopRust 10 0 >> obs.txt 2>> obs.err
	./nexus userRust 5 >> obs.txt 2>> obs.err
	./nexus secondMatchRust 5 >> obs.txt 2>> obs.err
	./nexus showAfterRust 5 >> obs.txt 2>> obs.err
	./nexus tryBodyRust 5 >> obs.txt 2>> obs.err
	./nexus onceCpp 5 >> obs.txt 2>> obs.err
	./nexus twiceCpp 5 >> obs.txt 2>> obs.err
	./nexus pairCpp 5 >> obs.txt 2>> obs.err
	./nexus loopCpp 10 0 >> obs.txt 2>> obs.err
	./nexus showAfterCpp 5 >> obs.txt 2>> obs.err
	./nexus showAfterRust 2 2>&1 | grep -o '"Ok":\[\[[0-9,]*\]\]' >> obs.txt
	./nexus showAfterCpp 2 2>&1 | grep -o '"Ok":\[\[[0-9,]*\]\]' >> obs.txt
	./nexus showLetCpp 5 >> obs.txt 2>> obs.err
	./nexus showLetCpp 2 2>&1 | grep -o '"Ok":\[\[[0-9,]*\]\]' >> obs.txt
	grep -c 'try_into_inner()' nexus-build/pools/rust/src/main.rs >> obs.txt
	grep -c "= std::move(std::get<" nexus-build/pools/cpp/pool.cpp >> obs.txt

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
