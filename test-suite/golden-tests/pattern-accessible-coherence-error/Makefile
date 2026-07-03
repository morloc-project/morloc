all:
	: > obs.txt
	rm -f *.err
	# The coherence check in Treeify.hs must reject the module before
	# typecheck completes. Capture the diagnostic and check for the
	# invariant-preserving substring; any wording change to the
	# check's error message will require updating this test.
	morloc typecheck main.loc > obs.txt 2>&1 || true
	grep -F "has a 'PatternAccessible' instance and also has" obs.txt > obs.txt.tmp
	mv obs.txt.tmp obs.txt

clean:
	rm -f *.err
