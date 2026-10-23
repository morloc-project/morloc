all:
	rm -f *.err obs.txt
	if morloc typecheck main.loc > obs.txt 2>&1; then \
	  echo "ERROR: typecheck unexpectedly succeeded" > obs.txt; \
	  exit 1; \
	fi
	grep -oE 'main\.loc:[0-9]+:[0-9]+' obs.txt | head -1 > obs.txt.tmp
	mv obs.txt.tmp obs.txt

clean:
	rm -f *.err
	rm -rf *-build *build.tmp*
