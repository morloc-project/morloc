all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus walk 2000 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt

clean:
	rm -rf nexus nexus-* pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
