MHOME := $${MORLOC_HOME:-$$HOME/.local/share/morloc}
NEXUS := $(MHOME)/bin/morloc-nexus

# `morloc-nexus file` footer-less recovery: the classifier keeps
# `state=missing` (the file on disk is unchanged), but a bounded
# forward walk of sub-packet headers surfaces `subpackets=N
# elements=N` fields. The cap (default 10,000, overridable via
# `MORLOC_FILE_MAX_SCAN_SUBPACKETS`) protects the fast-tool contract
# against pathological flush-every-element inputs; hitting the cap
# renders `N+` and a `hit sub-packet cap` note.

all:
	rm -f *.err
	: > obs.txt
	morloc make -o prog main.loc 2> build.err
	./prog mkStream stream.packet 2>> obs.err
	# Sanity: with the final footer present, `file` reports state=final
	# and knows the exact element count from StreamDiag.
	printf 'clean state: '  >> obs.txt
	$(NEXUS) file stream.packet | grep -oE 'state=[a-z]+' >> obs.txt 2>> obs.err
	printf 'clean elements: ' >> obs.txt
	$(NEXUS) file stream.packet | grep -oE 'elements=[0-9]+' >> obs.txt 2>> obs.err
	# Truncate the 8-byte EOF tail. The footer packet header is still
	# on disk but the classifier keys off the tail magic.
	cp stream.packet notail.packet
	N=`wc -c < notail.packet`; M=`expr $$N - 8`; \
	  dd if=stream.packet of=notail.packet bs=1 count=$$M status=none 2>> obs.err
	# `state=missing` (on-disk state unchanged), walk fields populated.
	printf 'notail state: '  >> obs.txt
	$(NEXUS) file notail.packet | grep -oE 'state=[a-z]+' >> obs.txt 2>> obs.err
	printf 'notail subpackets: '  >> obs.txt
	$(NEXUS) file notail.packet | grep -oE 'subpackets=[0-9]+\+?' >> obs.txt 2>> obs.err
	printf 'notail elements: '  >> obs.txt
	$(NEXUS) file notail.packet | grep -oE 'elements=[0-9]+\+?' | head -n 1 >> obs.txt 2>> obs.err
	printf 'notail footer_note: '  >> obs.txt
	$(NEXUS) file notail.packet | grep -oE 'footer_note="[^"]+"' >> obs.txt 2>> obs.err
	# Same file with the cap set below the actual sub-packet count.
	printf 'capped subpackets: '  >> obs.txt
	MORLOC_FILE_MAX_SCAN_SUBPACKETS=2 $(NEXUS) file notail.packet \
	  | grep -oE 'subpackets=[0-9]+\+?' >> obs.txt 2>> obs.err
	printf 'capped footer_note: '  >> obs.txt
	MORLOC_FILE_MAX_SCAN_SUBPACKETS=2 $(NEXUS) file notail.packet \
	  | grep -oE 'footer_note="[^"]+"' >> obs.txt 2>> obs.err
	# `file --json` exposes the walk under a `.walk` object.
	printf 'notail json scan_capped: '  >> obs.txt
	$(NEXUS) file --json notail.packet \
	  | grep -oE '"scan_capped":(true|false)' | head -n 1 >> obs.txt 2>> obs.err
	printf 'capped json scan_capped: '  >> obs.txt
	MORLOC_FILE_MAX_SCAN_SUBPACKETS=2 $(NEXUS) file --json notail.packet \
	  | grep -oE '"scan_capped":(true|false)' | head -n 1 >> obs.txt 2>> obs.err

clean:
	rm -rf prog pools __pycache__ *.err
	rm -f stream.packet notail.packet
