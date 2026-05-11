all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus --keep-null echoOptInt null > obs.txt 2> obs.err
	./nexus --keep-null echoOptInt 42 >> obs.txt 2>> obs.err
	./nexus --keep-null echoOptStr null >> obs.txt 2>> obs.err
	./nexus --keep-null echoOptStr '"hello"' >> obs.txt 2>> obs.err
	./nexus --keep-null echoOptList null >> obs.txt 2>> obs.err
	./nexus --keep-null echoOptList '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus --keep-null fromNullInt null >> obs.txt 2>> obs.err
	./nexus --keep-null fromNullInt 99 >> obs.txt 2>> obs.err
	./nexus --keep-null isNullInt null >> obs.txt 2>> obs.err
	./nexus --keep-null isNullInt 5 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
