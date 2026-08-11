all:
	: > obs.txt
	rm -f g1a.err g1b.err g1c.err g2a.err g2b.err
	@# grep -c exits 1 when the count is 0, so guard each with `|| true` to
	@# keep make going and write every obs.txt line (a 0 count is expected).
	@# Gate 1: a directly-written IO intrinsic is rejected under sandbox.
	morloc eval --eval-sandbox -e '@tell' 2> g1a.err || true
	grep -c "IO intrinsics may not be used directly" g1a.err >> obs.txt || true
	@# Gate 1: --eval-allowed-modules implies --eval-sandbox, so the same IO
	@# intrinsic is rejected even without an explicit --eval-sandbox flag.
	morloc eval --eval-allowed-modules root-py -e 'import root-py; @tell' 2> g1b.err || true
	grep -c "IO intrinsics may not be used directly" g1b.err >> obs.txt || true
	@# Gate 1: a non-IO intrinsic (@lang) is NOT rejected under sandbox.
	morloc eval --eval-sandbox -e '@lang' > /dev/null 2> g1c.err || true
	grep -c "IO intrinsics may not be used directly" g1c.err >> obs.txt || true
	@# Gate 2: a top-level import outside the allow-list is rejected. root-py
	@# resolves but is not in the allow-list {bits}, so the import is refused.
	morloc eval --eval-allowed-modules bits -e 'import root-py; 1 + 2' 2> g2a.err || true
	grep -c "is not in the eval allow-list" g2a.err >> obs.txt || true
	@# Gate 2: an allow-listed import is permitted and pure composition runs.
	@# root-py's own transitive imports are not checked (facade trust).
	printf 'allowed: ' >> obs.txt
	morloc eval --eval-allowed-modules root-py -e 'import root-py; 1 + 2' 2> g2b.err >> obs.txt || true

clean:
	rm -f g1a.err g1b.err g1c.err g2a.err g2b.err
