all:
	rm -f *.err obs.txt *.out
	morloc make -o nexus main.loc 2> build.err

	# Equivalence: streaming a list to @stdout renders identically to
	# returning the same list, per output format.
	./nexus -f json  @ returnInts > ri.out  2>> obs.err
	./nexus -f json  @ streamInts > si.out  2>> obs.err
	@echo "=== json equivalence (Int) ===" >> obs.txt
	@if cmp -s ri.out si.out; then echo MATCH >> obs.txt; else echo DIFFER >> obs.txt; diff ri.out si.out >> obs.txt; fi || true

	./nexus -f jsonl @ returnInts > rij.out 2>> obs.err
	./nexus -f jsonl @ streamInts > sij.out 2>> obs.err
	@echo "=== jsonl equivalence (Int) ===" >> obs.txt
	@if cmp -s rij.out sij.out; then echo MATCH >> obs.txt; else echo DIFFER >> obs.txt; diff rij.out sij.out >> obs.txt; fi || true

	./nexus -f json  @ returnStrs > rs.out  2>> obs.err
	./nexus -f json  @ streamStrs > ss.out  2>> obs.err
	@echo "=== json equivalence (Str) ===" >> obs.txt
	@if cmp -s rs.out ss.out; then echo MATCH >> obs.txt; else echo DIFFER >> obs.txt; diff rs.out ss.out >> obs.txt; fi || true

	./nexus -f jsonl @ returnStrs > rsj.out 2>> obs.err
	./nexus -f jsonl @ streamStrs > ssj.out 2>> obs.err
	@echo "=== jsonl equivalence (Str) ===" >> obs.txt
	@if cmp -s rsj.out ssj.out; then echo MATCH >> obs.txt; else echo DIFFER >> obs.txt; diff rsj.out ssj.out >> obs.txt; fi || true

	./nexus -f json  @ returnPairs > rp.out  2>> obs.err
	./nexus -f json  @ streamPairs > sp.out  2>> obs.err
	@echo "=== json equivalence (pairs) ===" >> obs.txt
	@if cmp -s rp.out sp.out; then echo MATCH >> obs.txt; else echo DIFFER >> obs.txt; diff rp.out sp.out >> obs.txt; fi || true

	./nexus -f jsonl @ returnPairs > rpj.out 2>> obs.err
	./nexus -f jsonl @ streamPairs > spj.out 2>> obs.err
	@echo "=== jsonl equivalence (pairs) ===" >> obs.txt
	@if cmp -s rpj.out spj.out; then echo MATCH >> obs.txt; else echo DIFFER >> obs.txt; diff rpj.out spj.out >> obs.txt; fi || true

	./nexus -f json  @ returnEmpty > re.out  2>> obs.err
	./nexus -f json  @ streamEmpty > se.out  2>> obs.err
	@echo "=== json equivalence (empty) ===" >> obs.txt
	@if cmp -s re.out se.out; then echo MATCH >> obs.txt; else echo DIFFER >> obs.txt; diff re.out se.out >> obs.txt; fi || true

	./nexus -f jsonl @ returnEmpty > rej.out 2>> obs.err
	./nexus -f jsonl @ streamEmpty > sej.out 2>> obs.err
	@echo "=== jsonl equivalence (empty) ===" >> obs.txt
	@if cmp -s rej.out sej.out; then echo MATCH >> obs.txt; else echo DIFFER >> obs.txt; diff rej.out sej.out >> obs.txt; fi || true

	# Concrete value: the empty stream must render as an empty JSON array.
	@echo "=== streamEmpty json ===" >> obs.txt
	./nexus -f json @ streamEmpty 2>> obs.err >> obs.txt

	# Format-independent sanity so a regression identical in BOTH paths
	# (which cmp cannot catch) still shows up.
	@echo "=== streamInts jsonl line count ===" >> obs.txt
	./nexus -f jsonl @ streamInts 2>> obs.err | wc -l | tr -d ' ' >> obs.txt
	@echo "=== streamInts jsonl sum ===" >> obs.txt
	./nexus -f jsonl @ streamInts 2>> obs.err | awk '{s+=$$1} END {print s}' >> obs.txt

	# Table/msgpack formats are rejected for streamed output.
	@echo "=== streamInts csv rejected ===" >> obs.txt
	( ./nexus -f csv @ streamInts >> obs.txt 2>> obs.err || echo REJECTED >> obs.txt )

clean:
	rm -rf nexus pools __pycache__ *.err *.out
