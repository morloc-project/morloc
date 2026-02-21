all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo '[["a","b"],[{"name":"alice","info":2},{"name":"bob","info":3}]]' | sed 's/ //g' > obs.txt 2> obs.err

clean:
	rm -rf nexus pools
