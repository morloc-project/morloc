MHOME := $${MORLOC_HOME:-$$HOME/.local/share/morloc}
NEXUS := $(MHOME)/bin/morloc-nexus

# `morloc-nexus view --pattern` exercised across DATA and STREAM packet
# shapes for the two list-shaped exports (flat + records), plus DATA-
# only chained access via the nested record.

all:
	rm -f *.err
	: > obs.txt
	morloc make -o prog main.loc 2> build.err
	# Materialise DATA packets from morloc program.
	./prog mkFlatList flat.packet    2>> obs.err
	./prog mkRecords  records.packet 2>> obs.err
	./prog mkNested   nested.packet  2>> obs.err
	# Convert DATA -> STREAM packets via `view -s` so patterns are
	# tested on both packet shapes.
	$(NEXUS) view flat.packet    -s -o flat.stream    2>> obs.err
	$(NEXUS) view records.packet -s -o records.stream 2>> obs.err

	# === Flat list of Int ===
	# Positive index, negative index (last), slice.
	printf 'flat DATA .[2]: '        >> obs.txt
	$(NEXUS) view flat.packet   --pattern '.[2]'   >> obs.txt 2>> obs.err
	printf 'flat STREAM .[2]: '      >> obs.txt
	$(NEXUS) view flat.stream   --pattern '.[2]'   >> obs.txt 2>> obs.err
	printf 'flat DATA .[-1]: '       >> obs.txt
	$(NEXUS) view flat.packet   --pattern '.[-1]'  >> obs.txt 2>> obs.err
	printf 'flat STREAM .[-1]: '     >> obs.txt
	$(NEXUS) view flat.stream   --pattern '.[-1]'  >> obs.txt 2>> obs.err
	printf 'flat DATA .[3:7]: '      >> obs.txt
	$(NEXUS) view flat.packet   --pattern '.[3:7]' >> obs.txt 2>> obs.err
	printf 'flat STREAM .[3:7]: '    >> obs.txt
	$(NEXUS) view flat.stream   --pattern '.[3:7]' >> obs.txt 2>> obs.err

	# === List of records ===
	# Bracket + field chain, group projection, slice.
	printf 'records DATA .[0].name: '     >> obs.txt
	$(NEXUS) view records.packet --pattern '.[0].name' >> obs.txt 2>> obs.err
	printf 'records STREAM .[0].name: '   >> obs.txt
	$(NEXUS) view records.stream --pattern '.[0].name' >> obs.txt 2>> obs.err
	printf 'records DATA .[1].age: '      >> obs.txt
	$(NEXUS) view records.packet --pattern '.[1].age'  >> obs.txt 2>> obs.err
	printf 'records STREAM .[1].age: '    >> obs.txt
	$(NEXUS) view records.stream --pattern '.[1].age'  >> obs.txt 2>> obs.err
	printf 'records DATA .[2].tags.[0]: ' >> obs.txt
	$(NEXUS) view records.packet --pattern '.[2].tags.[0]' >> obs.txt 2>> obs.err
	printf 'records STREAM .[2].tags.[0]: ' >> obs.txt
	$(NEXUS) view records.stream --pattern '.[2].tags.[0]' >> obs.txt 2>> obs.err
	printf 'records DATA .[0].(.name;.age): '   >> obs.txt
	$(NEXUS) view records.packet --pattern '.[0].(.name;.age)' >> obs.txt 2>> obs.err
	printf 'records STREAM .[0].(.name;.age): ' >> obs.txt
	$(NEXUS) view records.stream --pattern '.[0].(.name;.age)' >> obs.txt 2>> obs.err
	# List slice of records emitted via -f jsonl for a shape-agnostic
	# check (records are still records; slice preserves shape).
	printf 'records DATA .[0:2] jsonl lines: ' >> obs.txt
	$(NEXUS) view records.packet --pattern '.[0:2]' -f jsonl | wc -l >> obs.txt 2>> obs.err
	printf 'records STREAM .[0:2] jsonl lines: ' >> obs.txt
	$(NEXUS) view records.stream --pattern '.[0:2]' -f jsonl | wc -l >> obs.txt 2>> obs.err

	# === Nested record (DATA only; top level is not list-shaped) ===
	printf 'nested .label: '                >> obs.txt
	$(NEXUS) view nested.packet --pattern '.label'                    >> obs.txt 2>> obs.err
	printf 'nested .members.[0].name: '     >> obs.txt
	$(NEXUS) view nested.packet --pattern '.members.[0].name'         >> obs.txt 2>> obs.err
	printf 'nested .members.[1].tags.[0]: ' >> obs.txt
	$(NEXUS) view nested.packet --pattern '.members.[1].tags.[0]'     >> obs.txt 2>> obs.err

	# === Schema-mismatch errors (Rust walker rejects; exit 1) ===
	printf 'flat .foo exit: '   >> obs.txt
	$(NEXUS) view flat.packet --pattern '.foo' > /dev/null 2>tmp.err; printf '%s\n' "$$?" >> obs.txt
	printf 'flat .foo msg: '    >> obs.txt
	grep -oE 'expected record' tmp.err >> obs.txt 2>> obs.err
	rm -f tmp.err
	printf 'records .missing exit: ' >> obs.txt
	$(NEXUS) view records.packet --pattern '.[0].missing' > /dev/null 2>tmp.err; printf '%s\n' "$$?" >> obs.txt
	printf 'records .missing msg: '  >> obs.txt
	grep -oE 'field "missing" not found' tmp.err >> obs.txt 2>> obs.err
	rm -f tmp.err

	# === Parse errors (Rust parser rejects; exit 1) ===
	printf 'parse .[bad exit: ' >> obs.txt
	$(NEXUS) view flat.packet --pattern '.[bad' > /dev/null 2>tmp.err; printf '%s\n' "$$?" >> obs.txt
	printf 'parse .[bad msg: '  >> obs.txt
	grep -oE 'pattern parse|parse:' tmp.err | head -n 1 >> obs.txt 2>> obs.err
	rm -f tmp.err

clean:
	rm -rf prog pools __pycache__ *.err
	rm -f flat.packet records.packet nested.packet flat.stream records.stream
