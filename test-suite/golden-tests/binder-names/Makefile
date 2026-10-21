all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	for c in viaR viaPartial viaCross viaHelper viaBox viaPy viaRust viaRustRecord viaRustCross; do echo "$$c $$(./nexus $$c 3 2>> obs.err)" >> obs.txt; done
	echo "viaRustEnum $$(./nexus viaRustEnum North 2>> obs.err)" >> obs.txt
	./nexus viaFail 3 > /dev/null 2> fail.err; echo "viaFail exit=$$? $$(grep -q boom fail.err && echo boom || echo OTHER)" >> obs.txt
	./nexus viaClassed 3 > /dev/null 2> classed.err; echo "viaClassed exit=$$? $$(grep -q 'classed boom' classed.err && echo classed || echo OTHER)" >> obs.txt
	morloc make -o reserved reserved.loc 2>> build.err
	./reserved viaReserved 3 > /dev/null 2> reserved.err; echo "viaReserved exit=$$? $$(grep -q 'reserves: .__mm_99999' reserved.err && echo refused || echo OTHER)" >> obs.txt
	morloc make -o assign assign.loc 2>> build.err
	./assign viaAssign 3 > /dev/null 2> assign.err; echo "viaAssign exit=$$? $$(grep -q "locked binding for '.__mm_1'" assign.err && echo refused || echo OTHER)" >> obs.txt

clean:
	rm -rf nexus reserved assign pools *.err .RData __pycache__
	rm -rf *-build *build.tmp*
