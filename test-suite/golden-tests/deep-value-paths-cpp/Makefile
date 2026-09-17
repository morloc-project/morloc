MHOME := $${MORLOC_HOME:-$$HOME/.local/share/morloc}
NEXUS := $(MHOME)/bin/morloc-nexus

all:
	rm -f *.err c.json c0.pkt c3.pkt rptr.pkt c.mpk
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus chain 2 >> obs.txt 2>> obs.err
	./nexus chain 100000 > c.json 2>> obs.err
	./nexus count c.json >> obs.txt 2>> obs.err
	./nexus chain 100000 2>> obs.err | ./nexus count - >> obs.txt 2>> obs.err
	./nexus -f packet -z 0 -o c0.pkt chain 100000 2>> obs.err
	./nexus count c0.pkt >> obs.txt 2>> obs.err
	./nexus -f packet -z 3 -o c3.pkt chain 100000 2>> obs.err
	./nexus count c3.pkt >> obs.txt 2>> obs.err
	python3 -c "b=bytearray(open('c0.pkt','rb').read()); b[13]=2; open('rptr.pkt','wb').write(b)"
	./nexus count rptr.pkt > /dev/null 2> obs.rptr.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo rptr_rejected >> obs.txt
	grep -qF 'shared-memory reference' obs.rptr.err && echo rptr_named >> obs.txt || echo UNEXPECTED_MESSAGE >> obs.txt
	./nexus altCount 100000 >> obs.txt 2>> obs.err
	./nexus listIntLen 100000 >> obs.txt 2>> obs.err
	./nexus listStrLen 100000 >> obs.txt 2>> obs.err
	$(NEXUS) view c.json -f mpk --schema '&4Treev24Leaf04Node3i8^4Tree^4Tree' -o c.mpk 2>> obs.err
	./nexus count c.mpk >> obs.txt 2>> obs.err
	rm -f c.json c0.pkt c3.pkt rptr.pkt c.mpk

clean:
	rm -rf nexus pools __pycache__ *.err c.json c0.pkt c3.pkt rptr.pkt c.mpk
	rm -rf *-build *build.tmp*
