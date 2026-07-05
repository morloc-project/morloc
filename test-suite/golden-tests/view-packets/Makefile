MHOME := $${MORLOC_HOME:-$$HOME/.local/share/morloc}
NEXUS := $(MHOME)/bin/morloc-nexus

# The four-quadrant packet-conversion matrix of `morloc-nexus view`.
# Corpus: one DATA packet and one STREAM packet, both carrying the
# same 10-element [Int] payload. Every arm converts and then either
# byte-compares (DATA->DATA) or round-trips through `-f jsonl` for a
# value-level check.

all:
	rm -f *.err
	: > obs.txt
	morloc make -o prog main.loc 2> build.err
	./prog mkData   data.packet   2>> obs.err
	./prog mkStream stream.packet 2>> obs.err

	# === DATA -> DATA (byte-copy) ===
	$(NEXUS) view data.packet -d -o d2d.packet 2>> obs.err
	printf 'd2d byte-identity: '  >> obs.txt
	if cmp -s data.packet d2d.packet; then printf 'ok\n'; else printf 'differ\n'; fi >> obs.txt

	# === DATA -> STREAM ===
	$(NEXUS) view data.packet -s -o d2s.stream 2>> obs.err
	printf 'd2s file kind: '     >> obs.txt
	$(NEXUS) file -FD d2s.stream >> obs.txt 2>> obs.err
	printf 'd2s state: '         >> obs.txt
	$(NEXUS) file d2s.stream | grep -oE 'state=[a-z]+' >> obs.txt 2>> obs.err
	printf 'd2s jsonl lines: '   >> obs.txt
	$(NEXUS) view d2s.stream -f jsonl | wc -l >> obs.txt 2>> obs.err
	printf 'd2s jsonl sum via awk: ' >> obs.txt
	$(NEXUS) view d2s.stream -f jsonl | awk '{s+=$$1} END {print s}' >> obs.txt 2>> obs.err

	# === STREAM -> STREAM ===
	$(NEXUS) view stream.packet -s -o s2s.stream 2>> obs.err
	printf 's2s file kind: '     >> obs.txt
	$(NEXUS) file -FD s2s.stream >> obs.txt 2>> obs.err
	printf 's2s state: '         >> obs.txt
	$(NEXUS) file s2s.stream | grep -oE 'state=[a-z]+' >> obs.txt 2>> obs.err
	printf 's2s jsonl lines: '   >> obs.txt
	$(NEXUS) view s2s.stream -f jsonl | wc -l >> obs.txt 2>> obs.err
	printf 's2s jsonl sum: '     >> obs.txt
	$(NEXUS) view s2s.stream -f jsonl | awk '{s+=$$1} END {print s}' >> obs.txt 2>> obs.err

	# === STREAM -> DATA ===
	$(NEXUS) view stream.packet -d -o s2d.packet 2>> obs.err
	printf 's2d file kind: '     >> obs.txt
	$(NEXUS) file -FD s2d.packet >> obs.txt 2>> obs.err
	printf 's2d jsonl lines: '   >> obs.txt
	$(NEXUS) view s2d.packet -f jsonl | wc -l >> obs.txt 2>> obs.err
	printf 's2d jsonl sum: '     >> obs.txt
	$(NEXUS) view s2d.packet -f jsonl | awk '{s+=$$1} END {print s}' >> obs.txt 2>> obs.err

	# === -p on both inputs (preserve shape) ===
	$(NEXUS) view data.packet   -p -o pd.packet 2>> obs.err
	$(NEXUS) view stream.packet -p -o ps.stream 2>> obs.err
	printf 'pd file kind: '  >> obs.txt
	$(NEXUS) file -FD pd.packet >> obs.txt 2>> obs.err
	printf 'ps file kind: '  >> obs.txt
	$(NEXUS) file -FD ps.stream >> obs.txt 2>> obs.err

clean:
	rm -rf prog pools __pycache__ *.err
	rm -f data.packet stream.packet d2d.packet d2s.stream s2s.stream s2d.packet pd.packet ps.stream
