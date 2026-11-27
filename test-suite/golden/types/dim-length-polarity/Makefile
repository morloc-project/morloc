all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus scaleBy 2 '[1,2,3]' > obs.txt 2>&1 || true
	./nexus sumBelow 5 >> obs.txt 2>&1 || true
	./nexus sumThreePlus 10 >> obs.txt 2>&1 || true

clean:
	rm -rf nexus pools *err __pycache__
	rm -rf *-build *build.tmp*
