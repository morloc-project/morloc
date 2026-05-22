all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	# Rose of depth 3: perfect binary, 2**4 - 1 == 15 nodes.
	./nexus roseSizePy 3   >  obs.txt 2>  obs.err
	# Pat of depth 3: same shape, no payload. 15 nodes.
	./nexus patNodesPy 3   >> obs.txt 2>> obs.err
	# Pat depth 0: the single empty-list node. 1 node.
	./nexus patNodesPy 0   >> obs.txt 2>> obs.err
	# Test recursive functions
	./nexus llLen '[12,[23,[99,null]]]' >> obs.txt 2>> obs.err
	./nexus llSum '[12,[23,[99,null]]]' >> obs.txt 2>> obs.err
	# roseSum: 0 + 1 + 1 == 2
	./nexus roseSum '[0,[[1,[]],[1,[]]]]' >> obs.txt 2>> obs.err
	./nexus llRange 3 >> obs.txt 2>> obs.err
	# Native construction + JSON output for list-guarded shapes.
	./nexus roseBuild 2 >> obs.txt 2>> obs.err
	./nexus patBuild 2  >> obs.txt 2>> obs.err
	# JSON input via json_to_voidstar's Recur arm + native count.
	./nexus roseFromJson '[1,[]]'                >> obs.txt 2>> obs.err
	./nexus roseFromJson '[0,[[1,[]],[1,[]]]]'   >> obs.txt 2>> obs.err
	./nexus patFromJson '[]'                     >> obs.txt 2>> obs.err
	./nexus patFromJson '[[],[]]'                >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *err
