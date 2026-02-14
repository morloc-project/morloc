all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc > /dev/null 2> build.err
	./nexus processNumbers '[1,2,3]' > obs.txt 2> obs.err
	./nexus composeMaps '[5.0,10.0]' 2>> obs.err  >> obs.txt
	./nexus nestedLambda 10 '[1,2,3]' 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus pools
