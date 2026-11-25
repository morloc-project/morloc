all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus walk 1000000 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus walkMatch 1000000 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus walkTail 1000000 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus walkPast 10 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	grep -c "walked past the end" obs.err >> obs.txt

clean:
	rm -rf nexus nexus-* pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
