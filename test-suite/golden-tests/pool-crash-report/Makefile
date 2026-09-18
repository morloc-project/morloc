all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus crashPy 1 > /dev/null 2> obs.py.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo py_failed >> obs.txt
	grep -qE "morloc py pool: worker [0-9]+ crashed with signal 11" obs.py.err && echo py_signal >> obs.txt || echo NO_PY_SIGNAL >> obs.txt
	grep -qF "while running 'crashPy' (mid=" obs.py.err && echo py_command >> obs.txt || echo NO_PY_COMMAND >> obs.txt
	grep -qF "in m" obs.py.err && echo py_frames >> obs.txt || echo NO_PY_FRAMES >> obs.txt
	./nexus crashCpp 1 > /dev/null 2> obs.cpp.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo cpp_failed >> obs.txt
	grep -qF "Pool 'cpp' Pool process crashed with signal 11" obs.cpp.err && echo cpp_signal >> obs.txt || echo NO_CPP_SIGNAL >> obs.txt
	grep -qF "while executing crashCpp [cpp] (mid=" obs.cpp.err && echo cpp_manifold >> obs.txt || echo NO_CPP_MANIFOLD >> obs.txt
	./nexus viaCpp 1 > /dev/null 2> obs.via.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo via_failed >> obs.txt
	grep -qF "while executing _ [cpp] (mid=" obs.via.err && echo via_innermost >> obs.txt || echo NO_VIA_INNERMOST >> obs.txt
	grep -qF "while executing viaCpp" obs.via.err && echo UNEXPECTED_OUTER >> obs.txt || echo via_not_outer >> obs.txt
	./nexus crashRust 1 > /dev/null 2> obs.rust.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo rust_failed >> obs.txt
	grep -qF "Pool 'rust' Pool process crashed with signal 11" obs.rust.err && echo rust_signal >> obs.txt || echo NO_RUST_SIGNAL >> obs.txt
	grep -qF "while executing crashRust [rust] (mid=" obs.rust.err && echo rust_manifold >> obs.txt || echo NO_RUST_MANIFOLD >> obs.txt
	./nexus crashR 1 > /dev/null 2> obs.r.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo r_failed >> obs.txt
	grep -qE "morloc R pool: worker [0-9]+ (crashed with signal 11|exited with status)" obs.r.err && echo r_signal >> obs.txt || echo NO_R_SIGNAL >> obs.txt
	grep -qF "while running 'crashR' (mid=" obs.r.err && echo r_command >> obs.txt || echo NO_R_COMMAND >> obs.txt

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
