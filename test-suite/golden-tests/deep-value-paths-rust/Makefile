all:
	rm -f *.err chain.pkt
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus saveLoad 100000 chain.pkt >> obs.txt 2>> obs.err
	./nexus caterpillar 100000 >> obs.txt 2>> obs.err
	./nexus buildLL 3 >> obs.txt 2>> obs.err
	./nexus buildLL 100000 2>> obs.err | ./nexus llLen - >> obs.txt 2>> obs.err
	rm -f chain.pkt

clean:
	rm -rf nexus pools __pycache__ *.err chain.pkt
	rm -rf *-build *build.tmp*
