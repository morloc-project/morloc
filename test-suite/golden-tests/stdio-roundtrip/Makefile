all:
	rm -f *.err *.stream obs.txt
	morloc make -o nexus main.loc 2> build.err
	# Produce a stream-packet file, then read it back through a pipe.
	./nexus -f packet @ produce > pipe.stream 2>> obs.err
	@echo "=== file-roundtrip ===" >> obs.txt
	cat pipe.stream | ./nexus consume >> obs.txt 2>> obs.err
	# Direct pipe (no intermediate file).
	@echo "=== pipe-roundtrip ===" >> obs.txt
	./nexus -f packet @ produce 2>> obs.err | ./nexus consume >> obs.txt 2>> obs.err
	# EOF on stdin: three @next calls all see empty, sum = 0.
	@echo "=== empty-stdin ===" >> obs.txt
	./nexus consume < /dev/null >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err *.stream
