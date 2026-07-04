MHOME := $${MORLOC_HOME:-$$HOME/.local/share/morloc}
NEXUS := $(MHOME)/bin/morloc-nexus

# `morloc-nexus view -` streaming from stdin. The source is a morloc
# program that writes an [Int] stream to @stdout with 4 sub-packets
# separated by @flush. The Makefile pipes that stream into `view -`
# under different output forms and checks the results.

all:
	rm -f *.err
	: > obs.txt
	morloc make -o prog main.loc 2> build.err

	# === -f jsonl: one element per line, streamed sub-packet by
	#     sub-packet. Expected count = 3+4+2+1 = 10 lines.
	printf 'jsonl line count: ' >> obs.txt
	./prog emit | $(NEXUS) view - -f jsonl | wc -l >> obs.txt 2>> obs.err
	printf 'jsonl first line: ' >> obs.txt
	./prog emit | $(NEXUS) view - -f jsonl | head -n 1 >> obs.txt 2>> obs.err
	printf 'jsonl last line: '  >> obs.txt
	./prog emit | $(NEXUS) view - -f jsonl | tail -n 1 >> obs.txt 2>> obs.err
	printf 'jsonl sum: '        >> obs.txt
	./prog emit | $(NEXUS) view - -f jsonl \
	  | awk '{s+=$$1} END {print s}' >> obs.txt 2>> obs.err

	# === -s: stream-preserving copy. The output stream should be
	#     a well-formed stream-packet with the same 10 elements.
	./prog emit | $(NEXUS) view - -s -o out.stream 2>> obs.err
	printf 'stream file kind: ' >> obs.txt
	$(NEXUS) file -FD out.stream >> obs.txt 2>> obs.err
	printf 'stream state: '     >> obs.txt
	$(NEXUS) file out.stream | grep -oE 'state=[a-z]+' >> obs.txt 2>> obs.err
	printf 'stream elements: '  >> obs.txt
	$(NEXUS) file out.stream | grep -oE 'elements=[0-9]+' >> obs.txt 2>> obs.err
	# Round-trip check: view the -s output as jsonl and confirm
	# the values match the input.
	printf 'stream jsonl round-trip sum: ' >> obs.txt
	$(NEXUS) view out.stream -f jsonl \
	  | awk '{s+=$$1} END {print s}' >> obs.txt 2>> obs.err

clean:
	rm -rf prog pools __pycache__ *.err
	rm -f out.stream
