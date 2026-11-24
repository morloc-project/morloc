BAD = python3 -c 'import sys; b = bytearray(open(sys.argv[1], "rb").read()); b[-1] = 4; open(sys.argv[2], "wb").write(b)'
REFUSED = if grep -q 'enum tag 4 is out of range' obs.err; then echo refused >> obs.txt; else echo ACCEPTED >> obs.txt; fi

all:
	rm -f *.err obs.txt *.pkt
	morloc make -o nexus main.loc 2> build.err
	./nexus -f packet listComp '["A","C"]' > list.pkt 2>> obs.err
	$(BAD) list.pkt bad.pkt
	./nexus rListComp list.pkt > obs.txt 2>> obs.err
	./nexus cppListComp list.pkt >> obs.txt 2>> obs.err
	./nexus rustListComp list.pkt >> obs.txt 2>> obs.err
	: > obs.err; ./nexus rListComp bad.pkt >> obs.txt 2>> obs.err; $(REFUSED)
	: > obs.err; ./nexus listComp bad.pkt >> obs.txt 2>> obs.err; $(REFUSED)
	: > obs.err; ./nexus cppListComp bad.pkt >> obs.txt 2>> obs.err; $(REFUSED)
	: > obs.err; ./nexus rustListComp bad.pkt >> obs.txt 2>> obs.err; $(REFUSED)

clean:
	rm -rf nexus pools __pycache__ *.err *.pkt
	rm -rf *-build *build.tmp*
