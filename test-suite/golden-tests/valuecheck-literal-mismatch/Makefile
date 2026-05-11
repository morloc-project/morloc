all:
	rm -f *.err obs.txt
	# D4: value-checker error must put a newline between the "a:" and
	# "b:" lines (pre-fix the output was "a: 2b: 1").
	morloc make main.loc > obs.txt 2>&1 || true
	# Capture the two label lines verbatim.
	grep -E '^(a|b):' obs.txt > obs.txt.tmp
	mv obs.txt.tmp obs.txt

clean:
	rm -f *.err
	rm -rf main pools
