# Depths sit below each language's native ceiling: Python's recursion limit
# is 1000, and C++ and Rust natives reach about 130000 levels on a pool
# worker's 2 MiB stack. R's ceiling depends on its version and the stack
# (R before 4.4 recurses on the C stack), so the R depth is 80% of the
# native depth native-r.R finds on this host.
all:
	rm -f *.err obs.txt
	for l in py r cpp rs; do morloc make -o nexus-$$l main-$$l.loc 2>> build.err; done
	./nexus-py depth 900 >> obs.txt 2>> obs.err || echo "py failed" >> obs.txt
	n=$$(Rscript native-r.R) && d=$$((n * 4 / 5)) && [ "$$(./nexus-r depth $$d 2>> obs.err)" = "$$d" ] && echo "r reaches 80% of native" >> obs.txt || echo "r failed" >> obs.txt
	./nexus-cpp depth 100000 >> obs.txt 2>> obs.err || echo "cpp failed" >> obs.txt
	./nexus-rs depth 100000 >> obs.txt 2>> obs.err || echo "rs failed" >> obs.txt

clean:
	rm -rf nexus-* pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
