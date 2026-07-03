all:
	rm -f *.err obs.txt
	rm -f /tmp/shared-registry-double-*.dat
	morloc make -o nexus main.loc 2> build.err
	# The double-open MUST fail. We invert the shell exit: if
	# nexus succeeds (incorrect), echo "BUG: double-open allowed";
	# if it fails (correct), echo "rejected".
	-./nexus testDoubleOpen "/tmp/shared-registry-double-a.dat" > /dev/null 2>> obs.err && echo "BUG: double-open allowed" >> obs.txt || echo "rejected" >> obs.txt
	rm -f /tmp/shared-registry-double-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/shared-registry-double-*.dat
