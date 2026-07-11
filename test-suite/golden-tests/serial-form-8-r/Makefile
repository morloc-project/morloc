all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo '[["a",[["3",2.48],["4",1.2]]],["b",[["1",1.2],["2",2.48]]]]' | sed 's/ //g' > obs.txt 2> obs.err

clean:
	rm -rf nexus pools
