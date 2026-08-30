all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	# DIAGNOSTIC: capture each exit code and continue on error, so we learn
	# whether a missing result is a crash (non-zero exit -> a pool died, see the
	# CI crash reports) or a correctness/hang issue (exit 0 with wrong output).
	./nexus fact 0 > obs.txt 2> obs.err; echo "DIAG fact0 exit=$$?" >> obs.err
	./nexus fact 1 >> obs.txt 2>> obs.err; echo "DIAG fact1 exit=$$?" >> obs.err
	./nexus fact 5 >> obs.txt 2>> obs.err; echo "DIAG fact5 exit=$$?" >> obs.err

clean:
	rm -rf nexus pools *err
