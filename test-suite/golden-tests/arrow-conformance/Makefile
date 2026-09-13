PAIRS = py_py py_cpp py_r py_rust cpp_py cpp_cpp cpp_r cpp_rust r_py r_cpp r_r r_rust rust_py rust_cpp rust_r rust_rust
CASES = full one empty allnull

all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	for p in $(PAIRS); do for c in $(CASES); do echo "--- $$p $$c ---" >> obs.txt; ./nexus $$p $$c >> obs.txt 2>> obs.err; done; done

clean:
	rm -rf nexus pools *.err __pycache__
	rm -rf *-build *build.tmp*
