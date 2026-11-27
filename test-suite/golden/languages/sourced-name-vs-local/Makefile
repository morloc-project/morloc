all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus chain 0 > obs.txt 2> obs.err
	./nexus apply 0 >> obs.txt 2>> obs.err
	./nexus lmap '[0,1]' >> obs.txt 2>> obs.err
	./nexus wsum '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus wback '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus rchain 0 >> obs.txt 2>> obs.err
	./nexus rapply 0 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools
	rm -rf *-build *build.tmp*
	rm -f *.err
