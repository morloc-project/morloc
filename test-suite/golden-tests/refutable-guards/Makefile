all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo 0 >> obs.txt 2>> obs.err
	./nexus foo 5 >> obs.txt 2>> obs.err
	./nexus foo 9 >> obs.txt 2>> obs.err
	./nexus foo 10 >> obs.txt 2>> obs.err
	./nexus foo 20 >> obs.txt 2>> obs.err
	./nexus cmp '[1,2]' >> obs.txt 2>> obs.err
	./nexus cmp '[5,5]' >> obs.txt 2>> obs.err
	./nexus cmp '[9,3]' >> obs.txt 2>> obs.err
	./nexus sign -4 >> obs.txt 2>> obs.err
	./nexus sign 0 >> obs.txt 2>> obs.err
	./nexus sign 7 >> obs.txt 2>> obs.err
	./nexus sign 17 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
