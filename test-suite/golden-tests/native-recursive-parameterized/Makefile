all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	# Balanced binary trie depth 3: 2**4 - 1 = 15 nodes
	./nexus trieSizePy 3       >  obs.txt 2>  obs.err
	# Balanced binary TreeKV depth 3: also 15
	./nexus treeSizePy 3       >> obs.txt 2>> obs.err
	# Two-level trie with branching 4: 5 nodes
	./nexus twoLevelTriePy 4   >> obs.txt 2>> obs.err
	# Native construction + JSON output for parameterized recursive records.
	./nexus trieBuild 2        >> obs.txt 2>> obs.err
	./nexus treeKVBuild 2      >> obs.txt 2>> obs.err
	# JSON input via json_to_voidstar's Recur arm.
	./nexus trieFromJson '{"val":null,"subs":[["L",{"val":1,"subs":[]}],["R",{"val":2,"subs":[]}]]}'                                        >> obs.txt 2>> obs.err
	./nexus treeKVFromJson '{"key":"root","value":0,"children":[["a",{"key":"leaf","value":1,"children":[]}]]}'                             >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *err
