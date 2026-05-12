all:
	rm -f *.err obs.txt
	# C5: .0 / .N on a list (which would return runtime garbage)
	# must be rejected at typecheck, not silently accepted.
	morloc typecheck main.loc > obs.txt 2>&1 || true
	grep -F 'requires a tuple or record, got a list' obs.txt > obs.txt.tmp
	mv obs.txt.tmp obs.txt

clean:
	rm -f *.err
