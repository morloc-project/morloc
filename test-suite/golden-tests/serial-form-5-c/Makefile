all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo '[[["a",1],["b",5]],[["c",2],["d",6]]]' | sed 's/ //g' > obs.txt 2> obs.err

clean:
	rm -rf nexus pools
