MHOME := $${MORLOC_HOME:-$$HOME/.local/share/morloc}
NEXUS := $(MHOME)/bin/morloc-nexus

# The stream-classification golden test. Every assertion pulls a
# deterministic substring (state, status, subkind, schema, subpackets=N)
# rather than the full renderer output so exact byte counts and
# timestamps don't destabilize the expected file.

all:
	rm -f *.err obs.txt
	morloc make -o prog main.loc 2> build.err
	./prog writeClosed     stream-final.packet 2>> obs.err
	./prog writeTempFooter stream-temp.packet  2>> obs.err
	# Simulate a stream whose 8-byte EOF tail never made it to disk
	# (writer died between the temp-footer body and the tail write).
	# Copy the temp-footer file and strip its last 8 bytes; what
	# remains is header + subpackets + temp-footer body, no tail
	# magic. The classifier reports state=missing.
	N=`wc -c < stream-temp.packet`; M=`expr $$N - 8`; \
	  dd if=stream-temp.packet of=stream-missing.packet \
	     bs=1 count=$$M status=none 2>> obs.err
	# === plain classification: state extraction ===
	printf 'final state: '   >> obs.txt
	$(NEXUS) file stream-final.packet   | grep -o 'state=[a-z]*'   >> obs.txt 2>> obs.err
	printf 'final status: '  >> obs.txt
	$(NEXUS) file stream-final.packet   | grep -o 'status=[a-z]*'  >> obs.txt 2>> obs.err
	printf 'final subpackets: ' >> obs.txt
	$(NEXUS) file stream-final.packet   | grep -o 'subpackets=[0-9]*' >> obs.txt 2>> obs.err
	printf 'final has schema field: '  >> obs.txt
	$(NEXUS) file stream-final.packet   | grep -c 'schema="' >> obs.txt 2>> obs.err
	printf 'final token: '   >> obs.txt
	$(NEXUS) file -FD stream-final.packet >> obs.txt 2>> obs.err
	printf 'temp state: '    >> obs.txt
	$(NEXUS) file stream-temp.packet    | grep -o 'state=[a-z]*'   >> obs.txt 2>> obs.err
	printf 'temp subpackets: ' >> obs.txt
	$(NEXUS) file stream-temp.packet    | grep -o 'subpackets=[0-9]*' >> obs.txt 2>> obs.err
	printf 'missing state: ' >> obs.txt
	$(NEXUS) file stream-missing.packet | grep -o 'state=[a-z]*'   >> obs.txt 2>> obs.err
	printf 'missing note: '  >> obs.txt
	$(NEXUS) file stream-missing.packet | grep -oE 'no footer at EOF' >> obs.txt 2>> obs.err
	printf 'missing token: ' >> obs.txt
	$(NEXUS) file -FD stream-missing.packet >> obs.txt 2>> obs.err
	# Exit codes: all three classifications should succeed (0), because
	# `missing` is an unusual state but not a classifier error.
	printf 'final exit: '    >> obs.txt
	$(NEXUS) file stream-final.packet   > /dev/null 2>>obs.err; printf '%s\n' "$$?" >> obs.txt
	printf 'temp exit: '     >> obs.txt
	$(NEXUS) file stream-temp.packet    > /dev/null 2>>obs.err; printf '%s\n' "$$?" >> obs.txt
	printf 'missing exit: '  >> obs.txt
	$(NEXUS) file stream-missing.packet > /dev/null 2>>obs.err; printf '%s\n' "$$?" >> obs.txt
	# === JSON classification ===
	printf 'json final subkind: '   >> obs.txt
	$(NEXUS) file --json stream-final.packet   | grep -o '"subkind":"[^"]*"'  >> obs.txt 2>> obs.err
	printf 'json final state: '     >> obs.txt
	$(NEXUS) file --json stream-final.packet   | grep -o '"state":"[^"]*"'    >> obs.txt 2>> obs.err
	printf 'json final status: '    >> obs.txt
	$(NEXUS) file --json stream-final.packet   | grep -o '"status":"[^"]*"'   >> obs.txt 2>> obs.err
	printf 'json missing state: '   >> obs.txt
	$(NEXUS) file --json stream-missing.packet | grep -o '"state":"[^"]*"'    >> obs.txt 2>> obs.err
	# === Verbose (`-v`) dumps the diag fields ===
	# `grep -c` exits 1 on zero matches even though it prints "0" to
	# stdout, so append `|| true` to keep make happy on the intentional
	# zero-match case.
	printf 'verbose final has writer_pid: '  >> obs.txt
	$(NEXUS) file -v stream-final.packet | { grep -c 'writer_pid=' || true; } >> obs.txt 2>> obs.err
	printf 'verbose final has largest_packet: ' >> obs.txt
	$(NEXUS) file -v stream-final.packet | { grep -c 'largest_packet_uncompressed=' || true; } >> obs.txt 2>> obs.err
	printf 'verbose missing has no writer_pid: ' >> obs.txt
	$(NEXUS) file -v stream-missing.packet | { grep -c 'writer_pid=' || true; } >> obs.txt 2>> obs.err
	# === `--validate` on a stream must yield structure-only ===
	printf 'validate final: '   >> obs.txt
	$(NEXUS) file --validate stream-final.packet   | grep -o 'validated=[a-z-]*' >> obs.txt 2>> obs.err

clean:
	rm -rf prog pools __pycache__ *.err stream-final.packet stream-temp.packet stream-missing.packet
