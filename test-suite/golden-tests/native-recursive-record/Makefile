all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	# 2 ** 3 = 8 leaves -> sumPy 3 == 8 in every language
	./nexus sumPy 3            >  obs.txt 2>  obs.err
	./nexus sumCpp 3           >> obs.txt 2>> obs.err
	./nexus sumR 3             >> obs.txt 2>> obs.err
	# 5-element linked list -> length 5 in every language
	./nexus llPy 5             >> obs.txt 2>> obs.err
	./nexus llCpp 5            >> obs.txt 2>> obs.err
	./nexus llR 5              >> obs.txt 2>> obs.err
	# Py -> Cpp -> Py round-trip on a recursive value, then sum locally
	./nexus pingPyCpp 4        >> obs.txt 2>> obs.err
	# Native morloc length over a parameterized recursive record.
	./nexus containerLenPy 5   >> obs.txt 2>> obs.err
	# The native-construction + JSON I/O commands are commented out
	# pending the C++ struct-layout redesign documented in main.loc.
	# ./nexus llBuild 3          >> obs.txt 2>> obs.err
	# ./nexus treeBuild 2        >> obs.txt 2>> obs.err
	# ./nexus containerBuild 3   >> obs.txt 2>> obs.err
	# ./nexus llFromJson '{"head":1,"tail":{"head":2,"tail":{"head":3,"tail":null}}}'                                 >> obs.txt 2>> obs.err
	# ./nexus treeFromJson '{"value":5,"children":[{"value":3,"children":[]},{"value":4,"children":[]}]}'             >> obs.txt 2>> obs.err
	# ./nexus containerFromJson '{"val":10,"sub":{"val":20,"sub":null}}'                                              >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *err
