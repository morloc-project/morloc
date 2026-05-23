all:
	rm -f *.err obs.txt
	# This test EXPECTS morloc make to FAIL on a parameterized recursive
	# record whose general definition exists but has no concrete language
	# instance. Three conditions matter:
	#   (1) morloc make exits non-zero (the missing-instance check fires);
	#   (2) it writes a diagnostic to STDERR (any message);
	#   (3) it does NOT loop -- a 5-second timeout guards against
	#       regression of the inferConcreteVar gscopeUni self-recursive
	#       loop guard.
	# We deliberately don't assert anything about the diagnostic text;
	# the wording is free to change, only the failure shape is fixed.
	#
	# Capture STDOUT and STDERR separately. The test passes iff:
	#   exit != 0, exit != 124 (no timeout), AND stderr is non-empty.
	timeout 5s morloc make -o /tmp/mcre_nexus main.loc \
	  > obs.txt 2> obs.err ; rc=$$? ; \
	  rm -f /tmp/mcre_nexus /tmp/mcre_nexus.manifest ; \
	  if [ $$rc -eq 0 ]; then \
	    echo "FAIL: morloc make unexpectedly succeeded" > obs.txt ; exit 1 ; \
	  elif [ $$rc -eq 124 ]; then \
	    echo "FAIL: morloc make diverged (timed out at 5s); inferConcreteVar loop guard regressed" > obs.txt ; exit 1 ; \
	  elif [ ! -s obs.err ]; then \
	    echo "FAIL: morloc make exited $$rc but wrote nothing to STDERR" > obs.txt ; exit 1 ; \
	  else \
	    echo "PASS" > obs.txt ; \
	  fi

clean:
	rm -f *.err /tmp/mcre_nexus /tmp/mcre_nexus.manifest
