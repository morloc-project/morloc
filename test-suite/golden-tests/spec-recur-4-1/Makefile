# Depths sit below each language's native ceiling: Python's recursion limit
# is 1000, R's expressions limit 5000, and C++ and Rust natives reach about
# 130000 levels on a pool worker's 2 MiB stack.
all:
	rm -f *.err obs.txt
	for l in py r cpp rs; do morloc make -o nexus-$$l main-$$l.loc 2>> build.err; done
	./nexus-py depth 900 >> obs.txt 2>> obs.err || echo "py failed" >> obs.txt
	./nexus-r depth 4000 >> obs.txt 2>> obs.err || echo "r failed" >> obs.txt
	./nexus-cpp depth 100000 >> obs.txt 2>> obs.err || echo "cpp failed" >> obs.txt
	./nexus-rs depth 100000 >> obs.txt 2>> obs.err || echo "rs failed" >> obs.txt

clean:
	rm -rf nexus-* pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
