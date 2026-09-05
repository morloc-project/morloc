all:
	: > obs.txt
	morloc make -X futhark:backend=multicore -o nexus main.loc 2> build.err
	./nexus mmSum >> obs.txt 2> obs.err
	./nexus vScaledSum 2.0 '[1,2,3,4]' >> obs.txt 2>> obs.err
	(grep -q 'futhark multicore --library' build.err && echo backend-cmd-ok || echo backend-cmd-MISSING) >> obs.txt
	(grep -q 'futhark:backend=multicore' nexus-build/manifest.json && echo build-params-ok || echo build-params-MISSING) >> obs.txt

clean:
	rm -rf nexus pools build.err obs.err __pycache__
	rm -rf *-build *build.tmp*
