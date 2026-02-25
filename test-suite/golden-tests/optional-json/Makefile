all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus echoOptInt null > obs.txt 2> obs.err
	./nexus echoOptInt 42 >> obs.txt 2>> obs.err
	./nexus echoOptStr null >> obs.txt 2>> obs.err
	./nexus echoOptStr '"hello"' >> obs.txt 2>> obs.err
	./nexus echoOptList null >> obs.txt 2>> obs.err
	./nexus echoOptList '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus fromNullInt null >> obs.txt 2>> obs.err
	./nexus fromNullInt 99 >> obs.txt 2>> obs.err
	./nexus isNullInt null >> obs.txt 2>> obs.err
	./nexus isNullInt 5 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
