all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus allTags '[{"key":1,"tags":[10,20]},{"key":2,"tags":[30]}]' >> obs.txt 2> obs.err

clean:
	rm -rf nexus pools
	rm -rf *-build *build.tmp*
