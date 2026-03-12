all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus py_calls_cpp_err '"hello"' 2>&1 | grep -c "CPPERR" > obs.txt || echo "0" > obs.txt
	./nexus r_calls_py_err '"hello"' 2>&1 | grep -c "PYERR" >> obs.txt || echo "0" >> obs.txt

clean:
	rm -rf nexus pools* __pycache__ *.err obs.txt .manifest
