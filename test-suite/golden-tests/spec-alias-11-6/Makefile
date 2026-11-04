all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus mk 4 > obs.txt 2> obs.err
	./nexus len3 '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus len3 '[1,2]' >> obs.txt 2>&1 || echo "exit $$?" >> obs.txt
	./nexus len4 '[1,2,3,4]' >> obs.txt 2>> obs.err
	./nexus len4 '[1,2,3]' >> obs.txt 2>&1 || echo "exit $$?" >> obs.txt
	./nexus lenE '[1,2]' >> obs.txt 2>&1 || echo "exit $$?" >> obs.txt

clean:
	rm -rf nexus pools *err __pycache__
	rm -rf *-build *build.tmp*
