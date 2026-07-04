SHELL := /bin/bash
MHOME := $${MORLOC_HOME:-$$HOME/.local/share/morloc}
NEXUS := $(MHOME)/bin/morloc-nexus

all:
	: > obs.txt
	: > obs.err
	rm -f *.err.old *.tmp
	morloc make -o nexus main.loc 2> build.err
	# dataAndLog: @stdout and @stderr each carry their own stream
	# file. Independent SHM claim slots per stdio kind + independent
	# nexus-side mutexes.
	./nexus dataAndLog > data.tmp 2> log.tmp
	@echo "=== data_nonempty ===" >> obs.txt
	@if [ -s data.tmp ]; then echo "yes" >> obs.txt; else echo "no" >> obs.txt; fi
	@echo "=== log_nonempty ===" >> obs.txt
	@if [ -s log.tmp ]; then echo "yes" >> obs.txt; else echo "no" >> obs.txt; fi
	# `@close` on a stdio OStream must ship a final-footer packet
	# through the stdio RPC so `program > file.packet` produces a
	# well-formed stream file: state=final, status=closed, subpacket_
	# index populated, schema block intact. Assertions extract stable
	# substrings so exact byte counts don't destabilize exp.txt.
	@echo "=== data classification ===" >> obs.txt
	printf 'state: '      >> obs.txt
	$(NEXUS) file data.tmp | grep -o 'state=[a-z]*' >> obs.txt 2>> obs.err
	printf 'status: '     >> obs.txt
	$(NEXUS) file data.tmp | grep -o 'status=[a-z]*' >> obs.txt 2>> obs.err
	printf 'subpackets: ' >> obs.txt
	$(NEXUS) file data.tmp | grep -o 'subpackets=[0-9]*' >> obs.txt 2>> obs.err
	printf 'elements: '   >> obs.txt
	$(NEXUS) file data.tmp | grep -o 'elements=[0-9]*' >> obs.txt 2>> obs.err
	printf 'has_schema: ' >> obs.txt
	$(NEXUS) file data.tmp | grep -c 'schema="' >> obs.txt 2>> obs.err
	printf 'token: '      >> obs.txt
	$(NEXUS) file -FD data.tmp >> obs.txt 2>> obs.err
	printf 'validate: '   >> obs.txt
	$(NEXUS) file --validate data.tmp | grep -o 'validated=[a-z-]*' >> obs.txt 2>> obs.err
	@echo "=== log classification ===" >> obs.txt
	printf 'state: '      >> obs.txt
	$(NEXUS) file log.tmp | grep -o 'state=[a-z]*' >> obs.txt 2>> obs.err
	printf 'status: '     >> obs.txt
	$(NEXUS) file log.tmp | grep -o 'status=[a-z]*' >> obs.txt 2>> obs.err
	printf 'subpackets: ' >> obs.txt
	$(NEXUS) file log.tmp | grep -o 'subpackets=[0-9]*' >> obs.txt 2>> obs.err
	printf 'elements: '   >> obs.txt
	$(NEXUS) file log.tmp | grep -o 'elements=[0-9]*' >> obs.txt 2>> obs.err
	printf 'has_schema: ' >> obs.txt
	$(NEXUS) file log.tmp | grep -c 'schema="' >> obs.txt 2>> obs.err
	printf 'token: '      >> obs.txt
	$(NEXUS) file -FD log.tmp >> obs.txt 2>> obs.err
	printf 'validate: '   >> obs.txt
	$(NEXUS) file --validate log.tmp | grep -o 'validated=[a-z-]*' >> obs.txt 2>> obs.err
	@rm -f *.tmp

clean:
	rm -rf nexus pools __pycache__ *.err *.tmp
