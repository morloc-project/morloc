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

	# === Pattern + data-packet output ===
	# Pattern walker returns a voidstar with no accompanying packet
	# bytes; feeding that to the DATA_PACKET emitter used to segfault
	# because `normalize_data_packet_to_fd` was called with an empty
	# slice. Exercise both `-d -o file` and `-d` via stdout redirect
	# (the shape the user hit) and round-trip the result to prove
	# the emitted packet is well-formed.
	$(NEXUS) view flat.packet --pattern '.[3:7]' -d -o flat_slice.packet 2>> obs.err
	printf 'flat DATA .[3:7] -d file kind: ' >> obs.txt
	$(NEXUS) file -FD flat_slice.packet >> obs.txt 2>> obs.err
	printf 'flat DATA .[3:7] -d roundtrip jsonl lines: ' >> obs.txt
	$(NEXUS) view flat_slice.packet -f jsonl | wc -l >> obs.txt 2>> obs.err
	printf 'flat DATA .[3:7] -d roundtrip jsonl sum: ' >> obs.txt
	$(NEXUS) view flat_slice.packet -f jsonl | awk '{s+=$$1} END {print s}' >> obs.txt 2>> obs.err

	# STREAM input + pattern + -d, matching the user's original
	# repro shape (STREAM in, DATA slice out).
	$(NEXUS) view flat.stream --pattern '.[3:7]' -d -o flat_stream_slice.packet 2>> obs.err
	printf 'flat STREAM .[3:7] -d file kind: ' >> obs.txt
	$(NEXUS) file -FD flat_stream_slice.packet >> obs.txt 2>> obs.err
	printf 'flat STREAM .[3:7] -d roundtrip jsonl sum: ' >> obs.txt
	$(NEXUS) view flat_stream_slice.packet -f jsonl | awk '{s+=$$1} END {print s}' >> obs.txt 2>> obs.err

	# stdout redirect (`> file`) rather than `-o` -- this is the exact
	# CLI shape from the segfault report.
	$(NEXUS) view flat.stream --pattern '.[3:7]' -d > flat_stream_slice_redir.packet 2>> obs.err
	printf 'flat STREAM .[3:7] -d > redir file kind: ' >> obs.txt
	$(NEXUS) file -FD flat_stream_slice_redir.packet >> obs.txt 2>> obs.err
	printf 'flat STREAM .[3:7] -d > redir jsonl sum: ' >> obs.txt
	$(NEXUS) view flat_stream_slice_redir.packet -f jsonl | awk '{s+=$$1} END {print s}' >> obs.txt 2>> obs.err

	# Bare `-f packet` (no `-d`) with a pattern: same voidstar-only
	# code path, exercised through the alternate spelling.
	$(NEXUS) view flat.packet --pattern '.[3:7]' -f packet -o flat_slice_fpkt.packet 2>> obs.err
	printf 'flat DATA .[3:7] -f packet file kind: ' >> obs.txt
	$(NEXUS) file -FD flat_slice_fpkt.packet >> obs.txt 2>> obs.err
	printf 'flat DATA .[3:7] -f packet jsonl sum: ' >> obs.txt
	$(NEXUS) view flat_slice_fpkt.packet -f jsonl | awk '{s+=$$1} END {print s}' >> obs.txt 2>> obs.err

	# Scalar-shape pattern result (`.[2]` on flat list yields Int, not
	# [Int]) -- confirms the packet emitter handles non-list result
	# schemas too.
	$(NEXUS) view flat.packet --pattern '.[2]' -d -o flat_scalar.packet 2>> obs.err
	printf 'flat DATA .[2] -d file kind: ' >> obs.txt
	$(NEXUS) file -FD flat_scalar.packet >> obs.txt 2>> obs.err
	printf 'flat DATA .[2] -d roundtrip json: ' >> obs.txt
	$(NEXUS) view flat_scalar.packet >> obs.txt 2>> obs.err

	# Compressed data-packet emission (`-z 3`).
	$(NEXUS) view flat.packet --pattern '.[3:7]' -d -z 3 -o flat_slice_z.packet 2>> obs.err
	printf 'flat DATA .[3:7] -d -z file kind: ' >> obs.txt
	$(NEXUS) file -FD flat_slice_z.packet >> obs.txt 2>> obs.err
	printf 'flat DATA .[3:7] -d -z roundtrip jsonl sum: ' >> obs.txt
	$(NEXUS) view flat_slice_z.packet -f jsonl | awk '{s+=$$1} END {print s}' >> obs.txt 2>> obs.err

	# Record-list pattern result -- the record-list slice goes through
	# the same fd-write code path.
	$(NEXUS) view records.packet --pattern '.[0:2]' -d -o records_slice.packet 2>> obs.err
	printf 'records DATA .[0:2] -d file kind: ' >> obs.txt
	$(NEXUS) file -FD records_slice.packet >> obs.txt 2>> obs.err
	printf 'records DATA .[0:2] -d roundtrip jsonl lines: ' >> obs.txt
	$(NEXUS) view records_slice.packet -f jsonl | wc -l >> obs.txt 2>> obs.err

clean:
	rm -rf prog pools __pycache__ *.err
	rm -f flat.packet records.packet nested.packet flat.stream records.stream
	rm -f flat_slice.packet flat_stream_slice.packet flat_stream_slice_redir.packet
	rm -f flat_slice_fpkt.packet flat_scalar.packet flat_slice_z.packet records_slice.packet
