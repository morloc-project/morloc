FIX = sed 's/file .[^:]*:/file:/'

all:
	rm -f *.err obs.txt *.mpk *.vs
	morloc make -o nexus main.loc 2> build.err
	./nexus -f mpk mk3 > three.mpk 2>> obs.err
	./nexus -f voidstar mk3 > three.vs 2>> obs.err
	./nexus -f mpk mk2 > two.mpk 2>> obs.err
	./nexus -f voidstar mk2 > two.vs 2>> obs.err
	./nexus -f voidstar mkList > list.vs 2>> obs.err
	./nexus -f mpk mkList > list.mpk 2>> obs.err
	./nexus -f mpk mkRows > rows.mpk 2>> obs.err
	./nexus -f voidstar mkRows > rows.vs 2>> obs.err
	./nexus -f mpk mkPair > pair.mpk 2>> obs.err
	./nexus -f voidstar mkRec > rec.vs 2>> obs.err
	./nexus -f mpk mkOpt > opt.mpk 2>> obs.err
	./nexus -f voidstar bigRows 200000 > big.vs 2>> obs.err
	echo "--- json, wrong length ---" > obs.txt
	./nexus len2 '[1,2,3]' >> obs.txt 2>&1 || true
	echo "--- msgpack, wrong length ---" >> obs.txt
	./nexus len2 three.mpk 2>&1 | $(FIX) >> obs.txt || true
	echo "--- voidstar, wrong length ---" >> obs.txt
	./nexus len2 three.vs 2>&1 | $(FIX) >> obs.txt || true
	echo "--- voidstar list, wrong length ---" >> obs.txt
	./nexus len2 list.vs 2>&1 | $(FIX) >> obs.txt || true
	echo "--- msgpack list, wrong length ---" >> obs.txt
	./nexus len2 list.mpk 2>&1 | $(FIX) >> obs.txt || true
	echo "--- msgpack, nested in a list ---" >> obs.txt
	./nexus rows2 rows.mpk 2>&1 | $(FIX) >> obs.txt || true
	echo "--- voidstar, nested in a list ---" >> obs.txt
	./nexus rows2 rows.vs 2>&1 | $(FIX) >> obs.txt || true
	echo "--- msgpack, in a tuple ---" >> obs.txt
	./nexus pair2 pair.mpk 2>&1 | $(FIX) >> obs.txt || true
	echo "--- voidstar, in a record ---" >> obs.txt
	./nexus rec2 rec.vs 2>&1 | $(FIX) >> obs.txt || true
	echo "--- msgpack, in an optional ---" >> obs.txt
	./nexus opt2 opt.mpk 2>&1 | $(FIX) >> obs.txt || true
	echo "--- voidstar, large, last row wrong ---" >> obs.txt
	MORLOC_FRAME_WORKERS=4 ./nexus rows2 big.vs 2>&1 | $(FIX) >> obs.txt || true
	echo "--- msgpack, right length ---" >> obs.txt
	./nexus len2 two.mpk >> obs.txt 2>&1 || true
	echo "--- voidstar, right length ---" >> obs.txt
	./nexus len2 two.vs >> obs.txt 2>&1 || true

clean:
	rm -rf nexus pools *err __pycache__ *.mpk *.vs
	rm -rf *-build *build.tmp*
