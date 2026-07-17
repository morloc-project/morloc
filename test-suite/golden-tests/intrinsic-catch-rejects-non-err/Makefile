all:
	rm -f *.err obs.txt
	if morloc typecheck main.loc > raw.out 2>&1; then \
	  echo "FAIL: typecheck unexpectedly succeeded" > obs.txt ; \
	elif grep -q "@catch's first argument must have effect Err" raw.out ; then \
	  echo "PASS" > obs.txt ; \
	else \
	  echo "FAIL: typecheck failed but not with the expected @catch/Err diagnostic" > obs.txt ; \
	fi
	rm -f raw.out

clean:
	rm -f *.err raw.out
