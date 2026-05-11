all:
	rm -f *.err obs.txt
	# C10: pre-fix, a bare top-level binding (`val = body`) with an
	# effect mismatch caret'd at the export-list reference on line 1,
	# not at the body on line 10. Lock the caret onto the body line.
	morloc typecheck main.loc > obs.txt 2>&1 || true
	# Pull the file:line:col prefix from the first diagnostic line.
	grep -oE 'main\.loc:[0-9]+:[0-9]+' obs.txt | head -1 > obs.txt.tmp
	mv obs.txt.tmp obs.txt

clean:
	rm -f *.err
