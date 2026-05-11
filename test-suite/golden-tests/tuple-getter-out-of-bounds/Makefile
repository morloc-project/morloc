all:
	rm -f *.err obs.txt
	# C14: an out-of-bounds tuple-getter (.5 on a 2-tuple) must surface
	# a clear arity/index pair in the diagnostic.
	morloc typecheck main.loc > obs.txt 2>&1 || true
	# Capture just the key diagnostic line so the exp.txt is stable
	# against gensym names and pretty-printer changes.
	grep -o 'tuple arity [0-9]* required, index [0-9]* given' obs.txt > obs.txt.tmp
	mv obs.txt.tmp obs.txt

clean:
	rm -f *.err
