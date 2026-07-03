all:
	rm -f *.err
	: > obs.txt
	rm -f /tmp/istream-multi-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus testFirstSubpacket  "/tmp/istream-multi-a.dat" >> obs.txt 2>> obs.err
	./nexus testSecondSubpacket "/tmp/istream-multi-b.dat" >> obs.txt 2>> obs.err
	./nexus testThirdSubpacket  "/tmp/istream-multi-c.dat" >> obs.txt 2>> obs.err
	./nexus testEofAfterDrain   "/tmp/istream-multi-d.dat" >> obs.txt 2>> obs.err
	rm -f /tmp/istream-multi-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/istream-multi-*.dat
