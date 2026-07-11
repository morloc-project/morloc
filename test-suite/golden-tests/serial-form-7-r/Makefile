all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo '[["a",[1,2,3]],["b",[4,5,6]]]' | sed 's/ //g' > obs.txt 2> obs.err

clean:
	rm -rf nexus pools
