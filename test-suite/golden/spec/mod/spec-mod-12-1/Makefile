all:
	rm -f *.err obs.txt
	if morloc typecheck main.loc > obs.txt 2>&1; then \
	  echo "ERROR: typecheck unexpectedly succeeded" > obs.txt; \
	  exit 1; \
	fi
	grep -o "Illegal masking of type signatures for 'f'" obs.txt > obs.txt.tmp
	mv obs.txt.tmp obs.txt

clean:
	rm -f *.err
