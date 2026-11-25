MHOME := $${MORLOC_HOME:-$$HOME/.local/share/morloc}
NEXUS := $(MHOME)/bin/morloc-nexus

# FMT-6: a `view --pattern` group result is a tuple laid out by the C-struct
# rules, whatever the widths and alignments of its fields. Each case builds
# a packet from JSON with `--schema` and projects a group over it.

define run_case
	printf '%s %s: ' '$(1)' '$(2)' >> obs.txt
	$(NEXUS) view $(1) -f json --pattern '$(2)' >> obs.txt 2>> obs.err || echo FAILED >> obs.txt

endef

all:
	rm -f *.err *.packet *.stream
	: > obs.txt
	echo '[[1,"a"],[2,"b"]]' > is.json
	echo '[[1,1000],[2,2000]]' > ui.json
	echo '[[1.5,7],[2.5,8]]' > fi.json
	echo '[{"name":"Alice","age":30},{"name":"Bob","age":25}]' > rec.json
	echo '[{"p":{"a":1,"b":"x"}},{"p":{"a":2,"b":"y"}}]' > nest.json
	$(NEXUS) view is.json --schema at2i4s -f packet -o is.packet 2>> obs.err
	$(NEXUS) view ui.json --schema at2u1i4 -f packet -o ui.packet 2>> obs.err
	$(NEXUS) view fi.json --schema at2f8i1 -f packet -o fi.packet 2>> obs.err
	$(NEXUS) view rec.json --schema am24names3agei4 -f packet -o rec.packet 2>> obs.err
	$(NEXUS) view nest.json --schema am11pm21ai41bs -f packet -o nest.packet 2>> obs.err
	$(NEXUS) view is.packet -s -o is.stream 2>> obs.err
	$(call run_case,is.packet,.[0:2].(.0;.1))
	$(call run_case,is.packet,.[0:2].(.1;.0))
	$(call run_case,is.stream,.[0:2].(.0;.1))
	$(call run_case,ui.packet,.[0:2].(.0;.1))
	$(call run_case,ui.packet,.[0].(.1;.0))
	$(call run_case,fi.packet,.[0:2].(.1;.0))
	$(call run_case,rec.packet,.[0:2].(.name;.age))
	$(call run_case,rec.packet,.[1].(.age;.name))
	$(NEXUS) view ui.packet --pattern '.[0:2].(.1;.0)' -d -o ui-group.packet 2>> obs.err
	$(call run_case,ui-group.packet,.[1])
	$(call run_case,nest.packet,.[0:2].p.(.b;.a))
	rm -f *.json *.packet *.stream

clean:
	rm -f *.err *.packet *.stream *.json
