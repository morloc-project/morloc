# @collect output-formatter coverage across C++, Python, R.
#
# The same command suite (collect/main.loc) is collapsed into each language via
# main-<lang>.loc. exp-<lang>.txt must be regenerated from a verified obs run
# (the runtime is not buildable in the authoring environment). Binary -f
# outputs are round-tripped through `morloc-nexus view` rather than compared as
# raw bytes.

TMP := $(CURDIR)/gather-tmp

all: build cpp py r assertions

build:
	: > obs.txt
	rm -f *.err
	rm -rf $(TMP)
	mkdir -p $(TMP)
	morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err
	morloc make -o nexus-py  main-py.loc  2> build-py.err
	morloc make -o nexus-r   main-r.loc   2> build-r.err

# ---- Full -f matrix on the C++ reference -----------------------------------
# `run <nexus> <label>` exercises one language across every formatter and,
# for the `with` (whole) commands, every nexus -f option.
define run_lang
	printf -- '=== $(2): whole-form with (list, @load) under -f matrix ===\n' >> obs.txt
	./$(1) -f json    streamInts --sorted >> obs.txt 2>> obs.err
	./$(1) -f jsonl   streamInts --sorted >> obs.txt 2>> obs.err
	MORLOC_TMPDIR=$(TMP) ./$(1) -f voidstar streamInts --sorted 2>> obs.err | morloc-nexus view -f json - >> obs.txt 2>> obs.err
	MORLOC_TMPDIR=$(TMP) ./$(1) -f packet -z 9 streamInts --sorted 2>> obs.err | morloc-nexus view -f json - >> obs.txt 2>> obs.err
	printf -- 'mpk bytes: ' >> obs.txt ; MORLOC_TMPDIR=$(TMP) ./$(1) -f mpk streamInts --sorted 2>> obs.err | wc -c >> obs.txt
	printf -- '=== $(2): whole-form with (IFile, @flen) ===\n' >> obs.txt
	./$(1) -f json streamInts --summary >> obs.txt 2>> obs.err
	printf -- '=== $(2): with.buffer (per-batch) json/jsonl ===\n' >> obs.txt
	./$(1) -f json  streamInts --tagged >> obs.txt 2>> obs.err
	./$(1) -f jsonl streamInts --tagged >> obs.txt 2>> obs.err
	printf -- '=== $(2): with.buffer + offset (continuity 0,3,6) ===\n' >> obs.txt
	./$(1) -f jsonl streamInts --tag-off >> obs.txt 2>> obs.err
	printf -- '=== $(2): render.buffer (verbatim text) ===\n' >> obs.txt
	./$(1) streamInts --text >> obs.txt 2>> obs.err
	printf -- '=== $(2): render.buffer + offset ===\n' >> obs.txt
	./$(1) streamInts --text-off >> obs.txt 2>> obs.err
	printf -- '=== $(2): string element stream (sorted / count / join) ===\n' >> obs.txt
	./$(1) -f json streamStrs --sorted >> obs.txt 2>> obs.err
	./$(1) -f json streamStrs --count  >> obs.txt 2>> obs.err
	./$(1)         streamStrs --text   >> obs.txt 2>> obs.err
	printf -- '=== $(2): default @collect (no formatter) ===\n' >> obs.txt
	./$(1) -f jsonl streamInts >> obs.txt 2>> obs.err
endef

cpp:
	$(call run_lang,nexus-cpp,cpp)

py:
	$(call run_lang,nexus-py,py)

r:
	$(call run_lang,nexus-r,r)

# ---- Cross-cutting assertions (C++ reference) ------------------------------
assertions:
	# IFile-vs-list must agree on element count: summary == length of sorted.
	printf -- '=== IFile==list: summary vs json array length ===\n' >> obs.txt
	./nexus-cpp -f json streamInts --summary >> obs.txt 2>> obs.err
	# @close on a non-registered path must fail cleanly (not remove a file).
	printf -- '=== badClose (must error, not rm) ===\n' >> obs.txt
	if ./nexus-cpp badClose >> obs.txt 2>> obs.err; then \
		echo "UNEXPECTED: badClose succeeded" >> obs.txt; \
	else \
		echo "OK: @close on non-temp path rejected" >> obs.txt; \
	fi
	# Temp-file cleanup: after a loop of whole-form calls, no gather temps remain.
	printf -- '=== cleanup: gather temps after 5 whole-form calls ===\n' >> obs.txt
	for n in 1 2 3 4 5 ; do MORLOC_TMPDIR=$(TMP) ./nexus-cpp -f json streamInts --sorted >/dev/null 2>> obs.err ; done
	printf 'leftover morloc-gather files: ' >> obs.txt
	find $(TMP) -name 'morloc-gather-*' 2>/dev/null | wc -l >> obs.txt
	# Internal mlcp_* entries must stay hidden.
	printf -- '=== mlcp_ hidden from top-level --help (expect 0) ===\n' >> obs.txt
	./nexus-cpp --help 2>> obs.err | grep -c 'mlcp_' >> obs.txt ; true

clean:
	rm -rf nexus-cpp nexus-py nexus-r pools __pycache__ $(TMP) *.err
