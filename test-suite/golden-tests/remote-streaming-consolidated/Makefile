SHELL := /bin/bash

export LD_LIBRARY_PATH := $(HOME)/.local/share/morloc/lib:$(LD_LIBRARY_PATH)

# Scratch dir for handles-in-stream + file-based scenarios. The
# handle test creates three IFile datasets here; the file-based test
# writes a stream file here.
SCRATCH := $(shell mktemp -d -t remote-streaming-XXXXXX)

# Small buffer for the oversize scenario. 512 bytes forces the
# 50-element `[Int]` payload (400 raw + Array header + inline index)
# past the buffer, which routes append_one_element through its
# dedicated oversize branch.
SMALL_BUF := 512

all:
	rm -f *.err obs.txt build.err
	morloc make -o nexus main.loc 2> build.err
	@echo "=== tuples ===" >> obs.txt
	./nexus -f packet @ prodTuples 2>> obs.err | ./nexus consTuples >> obs.txt 2>> obs.err
	@echo "=== optional ===" >> obs.txt
	./nexus -f packet @ prodOpt 2>> obs.err | ./nexus consOpt >> obs.txt 2>> obs.err
	@echo "=== empty ===" >> obs.txt
	./nexus -f packet @ prodEmpty 2>> obs.err | ./nexus consEmpty >> obs.txt 2>> obs.err
	# @stdout ignores @write's compression arg; the nexus applies -z to
	# the recompressed stream, so drive compression with -z here.
	@echo "=== compression L1 ===" >> obs.txt
	./nexus -f packet -z 1 @ prodComp1 2>> obs.err | ./nexus consComp1 >> obs.txt 2>> obs.err
	@echo "=== compression L3 ===" >> obs.txt
	./nexus -f packet -z 3 @ prodComp3 2>> obs.err | ./nexus consComp3 >> obs.txt 2>> obs.err
	@echo "=== compression L7 ===" >> obs.txt
	./nexus -f packet -z 7 @ prodComp7 2>> obs.err | ./nexus consComp7 >> obs.txt 2>> obs.err
	@echo "=== handles ===" >> obs.txt
	./nexus prodHandles "$(SCRATCH)/handles.stream" \
	  "$(SCRATCH)/a.dat" "$(SCRATCH)/b.dat" "$(SCRATCH)/c.dat" 2>> obs.err
	./nexus consHandles "$(SCRATCH)/handles.stream" >> obs.txt 2>> obs.err
	@echo "=== oversize ===" >> obs.txt
	MORLOC_WRITE_BUFFER_BYTES=$(SMALL_BUF) ./nexus -f packet @ prodOversize 2>> obs.err \
	  | MORLOC_WRITE_BUFFER_BYTES=$(SMALL_BUF) ./nexus consOversize >> obs.txt 2>> obs.err
	@echo "=== file cross-invocation ===" >> obs.txt
	./nexus fileWrite "$(SCRATCH)/stream.dat" 2>> obs.err
	./nexus fileRead  "$(SCRATCH)/stream.dat" >> obs.txt 2>> obs.err
	rm -rf "$(SCRATCH)"

clean:
	rm -rf nexus pools __pycache__ *.err
	find /tmp -maxdepth 1 -name 'remote-streaming-*' -type d -exec rm -rf {} + 2>/dev/null || true
