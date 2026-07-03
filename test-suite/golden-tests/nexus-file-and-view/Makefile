MHOME := $${MORLOC_HOME:-$$HOME/.local/share/morloc}
NEXUS := $(MHOME)/bin/morloc-nexus

# This test exercises the new `morloc-nexus file` and `morloc-nexus view`
# subcommands. To keep the expected output stable across changes in
# msgpack/schema encoding sizes, every comparison either extracts a
# deterministic substring (subkind, kind, validated=X, exit code) or
# inspects only structurally fixed fields.

all:
	rm -f *.err
	: > obs.txt
	morloc make -o prog main.loc 2> build.err
	# Materialize a small corpus of data files.
	./prog -f packet     -o data.packet                              2>> obs.err
	./prog -f packet -z 5 -o data.packet.zst                         2>> obs.err
	printf '["a","b","c"]'        >  data.json
	$(NEXUS) view data.json -f mpk --schema "as" -o data.mpk                 2>> obs.err
	# Plain text README -- exercises the bug fix: ASCII text files
	# were being misclassified as MessagePack because every leading
	# byte is a valid msgpack fixint.
	printf 'README\n\nThis is a description.\nMore notes.\n' > readme.txt
	# Real CSV -- exercises the libmorloc `morloc_csv_infer` FFI path.
	printf 'name,age,city\nAlice,30,NYC\nBob,25,LA\n' > people.csv
	# Truncate the on-disk packet by 16 bytes so the header's declared
	# total disagrees with the file size; default `file` (no
	# --validate) must catch this.
	N=`wc -c < data.packet`; M=`expr $$N - 16`; dd if=data.packet of=truncated.packet bs=1 count=$$M status=none 2>> obs.err
	# === `file` plain output (deterministic subset) ===
	printf 'data.json kind: '             >> obs.txt
	$(NEXUS) file data.json | sed 's/^[^:]*: //' >> obs.txt 2>> obs.err
	printf 'data.mpk kind: '              >> obs.txt
	$(NEXUS) file data.mpk  | sed 's/^[^:]*: //' >> obs.txt 2>> obs.err
	printf 'readme.txt kind: '            >> obs.txt
	$(NEXUS) file readme.txt | sed 's/^[^:]*: //' >> obs.txt 2>> obs.err
	printf 'people.csv kind: '            >> obs.txt
	$(NEXUS) file people.csv | sed 's/^[^:]*: //' >> obs.txt 2>> obs.err
	printf 'missing.path type: '          >> obs.txt
	$(NEXUS) file missing.path | awk '{print $$2}' >> obs.txt 2>/dev/null
	printf 'missing.path msg: '           >> obs.txt
	$(NEXUS) file missing.path | grep -oE 'cannot open' >> obs.txt 2>/dev/null
	printf 'missing.path exit: '          >> obs.txt
	$(NEXUS) file missing.path > /dev/null 2>>obs.err; printf '%s\n' "$$?" >> obs.txt
	printf 'truncated.packet type: '      >> obs.txt
	$(NEXUS) file truncated.packet | awk '{print $$2}' >> obs.txt 2>/dev/null
	printf 'truncated.packet msg: '       >> obs.txt
	$(NEXUS) file truncated.packet | grep -oE 'truncated morloc data packet' >> obs.txt 2>/dev/null
	printf 'truncated.packet exit: '      >> obs.txt
	$(NEXUS) file truncated.packet > /dev/null 2>>obs.err; printf '%s\n' "$$?" >> obs.txt
	# === `file -F` (suppress file prefix) ===
	printf 'no-file data.json: '          >> obs.txt
	$(NEXUS) file -F data.json            >> obs.txt 2>> obs.err
	# === `file -FD` (just the type token) ===
	printf 'type-only data.packet: '      >> obs.txt
	$(NEXUS) file -FD data.packet         >> obs.txt 2>> obs.err
	printf 'type-only people.csv: '       >> obs.txt
	$(NEXUS) file -FD people.csv          >> obs.txt 2>> obs.err
	# === `file -v` (verbose: CSV column lines) ===
	printf 'verbose people.csv lines: '   >> obs.txt
	$(NEXUS) file -v people.csv | wc -l   >> obs.txt 2>> obs.err
	$(NEXUS) file -v people.csv | tail -n +2 | sort >> obs.txt 2>> obs.err
	# === `file --json` (extract just stable fields) ===
	printf 'json data.packet subkind: '   >> obs.txt
	$(NEXUS) file --json data.packet | grep -o '"subkind":"[^"]*"' >> obs.txt 2>> obs.err
	printf 'json data.packet schema: '    >> obs.txt
	$(NEXUS) file --json data.packet | grep -o '"schema":"[^"]*"'  >> obs.txt 2>> obs.err
	printf 'json data.json kind: '        >> obs.txt
	$(NEXUS) file --json data.json   | grep -o '"kind":"[^"]*"'    >> obs.txt 2>> obs.err
	# === `file --validate` ===
	printf 'validate data.packet: '       >> obs.txt
	$(NEXUS) file --validate data.packet     | grep -o 'validated=[a-z-]*' >> obs.txt 2>> obs.err
	printf 'validate data.packet.zst: '   >> obs.txt
	$(NEXUS) file --validate data.packet.zst | grep -o 'validated=[a-z-]*' >> obs.txt 2>> obs.err
	printf 'validate data.json: '         >> obs.txt
	$(NEXUS) file --validate data.json       | grep -o 'validated=[a-z-]*' >> obs.txt 2>> obs.err
	printf 'validate data.mpk --schema as: ' >> obs.txt
	$(NEXUS) file --validate --schema "as"  data.mpk | grep -o 'validated=[a-z-]*' >> obs.txt 2>> obs.err
	printf 'validate data.mpk --schema ai4: ' >> obs.txt
	$(NEXUS) file --validate --schema "ai4" data.mpk > tmp.out 2>>obs.err; printf 'exit=%s ' "$$?" >> obs.txt
	grep -o 'validated=[a-z]*' tmp.out >> obs.txt 2>> obs.err
	rm -f tmp.out
	# === `view` happy paths ===
	printf 'view data.packet -f json: '   >> obs.txt
	$(NEXUS) view data.packet -f json     >> obs.txt 2>> obs.err
	printf 'view data.packet.zst -f json: ' >> obs.txt
	$(NEXUS) view data.packet.zst -f json >> obs.txt 2>> obs.err
	printf 'view data.json -f mpk --schema as hex: ' >> obs.txt
	$(NEXUS) view data.json -f mpk --schema "as" | od -A n -t x1 | tr -d ' \n' >> obs.txt 2>> obs.err
	printf '\n' >> obs.txt
	printf 'view data.mpk no-schema exit: ' >> obs.txt
	$(NEXUS) view data.mpk > /dev/null 2>tmp.err; printf '%s\n' "$$?" >> obs.txt
	printf 'view data.mpk no-schema msg: ' >> obs.txt
	grep -o 'no schema available' tmp.err >> obs.txt 2>> obs.err
	rm -f tmp.err
	# View must reject a truncated morloc packet up-front instead of
	# decoding garbage from a partial payload.
	printf 'view truncated.packet exit: ' >> obs.txt
	$(NEXUS) view truncated.packet > /dev/null 2>tmp.err; printf '%s\n' "$$?" >> obs.txt
	printf 'view truncated.packet msg: ' >> obs.txt
	grep -o 'truncated morloc data packet' tmp.err >> obs.txt 2>> obs.err
	rm -f tmp.err
	# `-FD` produces a clean type stream (one type token per file).
	printf '%s' '-FD line count over corpus: '  >> obs.txt
	$(NEXUS) file -FD data.json data.mpk data.packet readme.txt people.csv | wc -l >> obs.txt 2>> obs.err

clean:
	rm -rf prog pools __pycache__ *.err *.packet *.packet.zst data.json data.mpk readme.txt people.csv truncated.packet tmp.out tmp.err
