all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus descend 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus descendOwned 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus pairs 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
