SHELL := /bin/bash
MHOME := $${MORLOC_HOME:-$$HOME/.local/share/morloc}
NEXUS := $(MHOME)/bin/morloc-nexus

define refused
	@if ./nexus $(1) >> obs.txt 2>> obs.err ; then echo "UNEXPECTED $(1)" >> obs.txt ; else echo "refused $(1)" >> obs.txt ; fi
endef

define piped
	@if $(1) | ./nexus sumStdin >> obs.txt 2>> obs.err ; then echo "UNEXPECTED $(2)" >> obs.txt ; else echo "refused $(2)" >> obs.txt ; fi
endef

all:
	: > obs.txt
	rm -f *.err *.pkt
	morloc make -o nexus main.loc 2> build.err
	./nexus -f packet -o ok.pkt ok 2>> obs.err
	./nexus loadAll ok.pkt >> obs.txt 2>> obs.err
	$(NEXUS) file --validate ok.pkt | grep -o "status=[a-z]*" >> obs.txt
	cp ok.pkt keep.pkt
	if ./nexus -f packet -o keep.pkt fails 2>> obs.err ; then echo UNEXPECTED >> obs.txt ; else cmp -s keep.pkt ok.pkt && echo "-o kept its file" >> obs.txt ; fi
	if ./nexus -f packet -o none.pkt fails 2>> obs.err ; then echo UNEXPECTED >> obs.txt ; else test -e none.pkt || echo "-o made no file" >> obs.txt ; fi
	ls -a | grep -c "morloc-" >> obs.txt ; true
	-./nexus -f packet fails > failed.pkt 2>> obs.err
	$(NEXUS) file failed.pkt | grep -o "status=[a-z]*" >> obs.txt
	if $(NEXUS) file --validate failed.pkt > /dev/null ; then echo UNEXPECTED >> obs.txt ; else echo "validate failed.pkt fails" >> obs.txt ; fi
	$(call refused,loadAll failed.pkt)
	$(call refused,first failed.pkt)
	$(call refused,fileLen failed.pkt)
	python3 cut.py ok.pkt
	./nexus loadAll nofooter.pkt >> obs.txt 2>> obs.err
	./nexus first nofooter.pkt >> obs.txt 2>> obs.err
	$(call refused,fileLen nofooter.pkt)
	if $(NEXUS) file --validate nofooter.pkt > /dev/null ; then echo UNEXPECTED >> obs.txt ; else echo "validate nofooter.pkt fails" >> obs.txt ; fi
	$(NEXUS) file --repair nofooter.pkt failed.pkt ok.pkt >> obs.txt
	./nexus loadAll nofooter.pkt >> obs.txt 2>> obs.err
	./nexus loadAll failed.pkt >> obs.txt 2>> obs.err
	./nexus fileLen nofooter.pkt >> obs.txt 2>> obs.err
	$(NEXUS) file --validate nofooter.pkt | grep -o "status=[a-z]*" >> obs.txt
	./nexus -f packet ok 2>> obs.err | ./nexus sumStdin >> obs.txt 2>> obs.err
	$(call piped,./nexus -f packet fails 2>> obs.err,a failed stream)
	$(call piped,cat infooter.pkt,a stream cut in its footer)
	$(call piped,cat inheader.pkt,a stream cut in a sub-packet header)
	$(call piped,head -c 272 ok.pkt,a stream with no footer)
	./nexus -f packet ok 2>> obs.err | $(NEXUS) view - -f jsonl | wc -l | tr -d ' ' >> obs.txt
	if ./nexus -f packet fails 2>> obs.err | $(NEXUS) view - -f jsonl > /dev/null 2>> obs.err ; then echo "UNEXPECTED view of a failed stream" >> obs.txt ; else echo "refused view of a failed stream" >> obs.txt ; fi
	if head -c 272 ok.pkt | $(NEXUS) view - -f jsonl > /dev/null 2>> obs.err ; then echo "UNEXPECTED view of a stream with no footer" >> obs.txt ; else echo "refused view of a stream with no footer" >> obs.txt ; fi

clean:
	rm -rf nexus pools __pycache__ *.err *.pkt
	rm -rf *-build *build.tmp*
