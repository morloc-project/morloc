all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	# Basic Pair Int: length 5 chain
	./nexus pairLenPy 5         >  obs.txt 2>  obs.err
	# Branching BTree at depth 3: 2**4 - 1 == 15 nodes
	./nexus btreeSizePy 3       >> obs.txt 2>> obs.err
	# btreeSum at depth 3: leaves carry 1, internals carry 0, so the
	# total sum equals the leaf count == 2**3 == 8.
	./nexus btreeSumPy 3        >> obs.txt 2>> obs.err
	# Two-parameter Pair2 Int Str: length 5
	./nexus pair2LenPy 5        >> obs.txt 2>> obs.err
	# Stress: 400-deep right-spined Pair, iteratively built in Py,
	# traversed recursively in morloc native code via the Py pool.
	./nexus pairLenDeepPy 400   >> obs.txt 2>> obs.err
	# Native construction + JSON output via to_json's Recur arm.
	./nexus pairBuild 3         >> obs.txt 2>> obs.err
	./nexus btreeBuild 2        >> obs.txt 2>> obs.err
	./nexus pair2Build 2        >> obs.txt 2>> obs.err
	# JSON input via json_to_voidstar's Recur arm + native count.
	./nexus pairFromJson '[3,[2,[1,[0,null]]]]'                                          >> obs.txt 2>> obs.err
	./nexus btreeFromJson '[0,[1,null,null],[1,null,null]]'                              >> obs.txt 2>> obs.err
	./nexus pair2FromJson '[1,"x",[0,"end",null]]'                                       >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *err
