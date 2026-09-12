all:
	rm -f *.err
	: > obs.txt
	rm -f /tmp/sshb-*.dat
	morloc make -o nexus main.loc 2> build.err
	echo "--- appendRecord ---" >> obs.txt
	./nexus appendRecord "/tmp/sshb-rec.dat" >> obs.txt 2>> obs.err
	echo "--- appendAlias ---" >> obs.txt
	./nexus appendAlias "/tmp/sshb-alias.dat" >> obs.txt 2>> obs.err
	echo "--- appendMany ---" >> obs.txt
	./nexus appendMany "/tmp/sshb-many.dat" >> obs.txt 2>> obs.err
	echo "--- readEv via stdin ---" >> obs.txt
	./nexus -f packet produceEv 7 | ./nexus readEv >> obs.txt 2>> obs.err
	echo "--- readMismatch ---" >> obs.txt
	if ./nexus readMismatch "/tmp/sshb-bad.dat" >> obs.err 2>&1 ; then \
	  echo "UNEXPECTED: mismatched element type was accepted" >> obs.txt ; \
	else \
	  echo "OK: mismatched element type rejected" >> obs.txt ; \
	fi
	rm -f /tmp/sshb-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/sshb-*.dat
	rm -rf *-build *build.tmp*
