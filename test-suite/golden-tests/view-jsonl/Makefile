MHOME := $${MORLOC_HOME:-$$HOME/.local/share/morloc}
NEXUS := $(MHOME)/bin/morloc-nexus

# `morloc-nexus view -f jsonl` streaming JSON-lines emitter.
# Corpus: mkListInt / mkListRecord / mkScalar / mkEmpty produced as
# data packets via @save; each is then re-emitted via `view -f jsonl`
# and structurally checked.

all:
	rm -f *.err
	: > obs.txt
	morloc make -o prog main.loc 2> build.err
	./prog mkListInt    list-int.packet    2>> obs.err
	./prog mkListRecord list-record.packet 2>> obs.err
	./prog mkScalar     scalar.packet      2>> obs.err
	./prog mkEmpty      empty.packet       2>> obs.err
	# === list of Int ===
	printf 'list-int line count: '  >> obs.txt
	$(NEXUS) view list-int.packet -f jsonl | wc -l >> obs.txt 2>> obs.err
	printf 'list-int first line: '  >> obs.txt
	$(NEXUS) view list-int.packet -f jsonl | head -n 1 >> obs.txt 2>> obs.err
	printf 'list-int last line: '   >> obs.txt
	$(NEXUS) view list-int.packet -f jsonl | tail -n 1 >> obs.txt 2>> obs.err
	# === list of Record ===
	printf 'list-record line count: '  >> obs.txt
	$(NEXUS) view list-record.packet -f jsonl | wc -l >> obs.txt 2>> obs.err
	printf 'list-record first name: '  >> obs.txt
	$(NEXUS) view list-record.packet -f jsonl | head -n 1 | grep -oE '"name":"[^"]*"' >> obs.txt 2>> obs.err
	printf 'list-record all rows have name+age: ' >> obs.txt
	# Each JSONL row must contain both fields; a `grep -c` = row count
	# means every line matched.
	$(NEXUS) view list-record.packet -f jsonl | grep -c '"name":.*"age":' >> obs.txt 2>> obs.err
	# === scalar (non-list): jsonl still emits exactly one line ===
	printf 'scalar line count: '   >> obs.txt
	$(NEXUS) view scalar.packet -f jsonl | wc -l >> obs.txt 2>> obs.err
	printf 'scalar value: '        >> obs.txt
	$(NEXUS) view scalar.packet -f jsonl >> obs.txt 2>> obs.err
	# === empty list: jsonl emits zero lines cleanly (no trailing null) ===
	printf 'empty line count: '    >> obs.txt
	$(NEXUS) view empty.packet -f jsonl | wc -l >> obs.txt 2>> obs.err

clean:
	rm -rf prog pools __pycache__ *.err
	rm -f list-int.packet list-record.packet scalar.packet empty.packet
