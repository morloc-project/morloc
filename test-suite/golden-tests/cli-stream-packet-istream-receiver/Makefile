all:
	rm -f *.err
	: > obs.txt
	rm -f /tmp/cli-stream-istream-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus writeStream /tmp/cli-stream-istream-a.dat >> obs.txt 2>> obs.err
	./nexus sumStream   /tmp/cli-stream-istream-a.dat >> obs.txt 2>> obs.err
	rm -f /tmp/cli-stream-istream-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/cli-stream-istream-*.dat
