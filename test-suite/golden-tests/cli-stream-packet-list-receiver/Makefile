all:
	rm -f *.err
	: > obs.txt
	rm -f /tmp/cli-stream-list-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus writeStream /tmp/cli-stream-list-a.dat >> obs.txt 2>> obs.err
	./nexus sumList     /tmp/cli-stream-list-a.dat >> obs.txt 2>> obs.err
	rm -f /tmp/cli-stream-list-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/cli-stream-list-*.dat
