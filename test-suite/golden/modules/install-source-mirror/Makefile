STATE := $(CURDIR)/state

all:
	rm -rf obs.txt *.err state
	MORLOC_STATE=$(STATE) morloc make --install -f mirrorprog/main.loc 2>> build.err
	$(STATE)/exe/mirrorprog/mirrorprog same 5 >> obs.txt 2>> obs.err
	cat $(STATE)/exe/mirrorprog/data/note.txt >> obs.txt
	test -f $(STATE)/exe/mirrorprog/main.loc && echo "main.loc at mirror root" >> obs.txt
	test ! -e $(STATE)/exe/mirrorprog/other && echo "sibling not mirrored" >> obs.txt
	test ! -e $(STATE)/exe/mirrorprog/mirrorprog/main.loc && echo "package not nested" >> obs.txt

clean:
	MORLOC_STATE=$(STATE) morloc uninstall mirrorprog
	rm -rf state *.err mirrorprog/*-build
