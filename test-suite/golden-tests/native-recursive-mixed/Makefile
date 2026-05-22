all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	# nodeReachPy 3: chain of 4 Nodes, each carrying Pair of length 4 == 16.
	./nexus nodeReachPy 3   >  obs.txt 2>  obs.err
	# wrappedLenPy 5: WrappedLL whose inner LL has length 6.
	./nexus wrappedLenPy 5  >> obs.txt 2>> obs.err
	# Native construction + JSON output, mixed record/tuple shapes.
	./nexus nodeBuild 2     >> obs.txt 2>> obs.err
	./nexus wrappedBuild 2  >> obs.txt 2>> obs.err
	# JSON input + native traversal across both record and tuple axes.
	./nexus nodeFromJson '{"ident":1,"chain":[7,[8,null]],"left":{"ident":0,"chain":[9,null],"left":null}}'   >> obs.txt 2>> obs.err
	./nexus wrappedFromJson '[42,{"head":1,"tail":{"head":2,"tail":null}}]'                                   >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *err
