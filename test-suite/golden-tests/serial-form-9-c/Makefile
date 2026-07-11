all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo '[["a",{"name":"alice","info":2}],["b",{"name":"bob","info":3}]]' | sed 's/ //g' > obs.txt 2> obs.err

clean:
	rm -rf nexus pools
