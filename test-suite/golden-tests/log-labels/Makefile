# Per-label logging (log: true) across all three pool languages.
# main.loc exports:
#   * mapsumP / mapsumC / mapsumR -- one per language; the trilang baseline.
#   * tupTest -- per-call-site labels on the same function (tup_left/tup_right).
#   * ifPos / ifNeg -- labels on if-then-else branches.
#   * nested -- nested labels on pointfree definitions (outer/inner).
#   * factP / factC / factR -- labeled recursive call (recur) per language.
#   * letTest -- labels in both the let-value and let-body positions.
#
# obs.txt = stdout (result values) + parsed counts of [morloc] start /
# done lines per label. Counting lines keeps the test robust to per-language
# formatting and timing values.

INPUT := '[[1,2,3.5],[3,4]]'

all:
	rm -f *.err obs.txt obs.*.err
	morloc make -o nexus main.loc 2> build.err
	./nexus mapsumP $(INPUT) >> obs.txt 2> obs.py.err
	./nexus mapsumC $(INPUT) >> obs.txt 2> obs.cpp.err
	./nexus mapsumR $(INPUT) >> obs.txt 2> obs.r.err
	./nexus tupTest $(INPUT) >> obs.txt 2> obs.tup.err
	./nexus ifPos   $(INPUT) >> obs.txt 2> obs.ifpos.err
	./nexus ifNeg   $(INPUT) >> obs.txt 2> obs.ifneg.err
	./nexus nested  $(INPUT) >> obs.txt 2> obs.nested.err
	./nexus factP   5        >> obs.txt 2> obs.factP.err
	./nexus factC   5        >> obs.txt 2> obs.factC.err
	./nexus factR   5        >> obs.txt 2> obs.factR.err
	./nexus letTest $(INPUT) >> obs.txt 2> obs.letTest.err
	cat obs.*.err > obs.err
	for lbl in a b tup_left tup_right then_lbl else_lbl then2 else2 outer inner recur let_in let_out; do \
	  echo "$$lbl: start=$$(grep -cE "^\[morloc\] $$lbl: start" obs.err) end=$$(grep -cE "^\[morloc\] $$lbl: (done in|FAILED after)" obs.err)" >> obs.txt; \
	done

clean:
	rm -rf nexus pools *.out obs.*.err
