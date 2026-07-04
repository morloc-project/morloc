MHOME := $${MORLOC_HOME:-$$HOME/.local/share/morloc}
NEXUS := $(MHOME)/bin/morloc-nexus

# Every guardrail on `morloc-nexus view`:
#   * Buffered-output size gate on stream input (`-f json` on stream).
#   * Stream -> data size gate (`-d` on stream).
#   * Binary-to-tty refusal (`-f voidstar` piped through a fake tty).
#   * CLI mutex / -f-vs-packet-flag / -p-on-non-packet errors.
# Each refusal is checked twice: bare (exit 1 + refusal message) and
# with `--force` (exit 0). Size thresholds are shrunk via
# `MORLOC_VIEW_MAX_BUFFER_BYTES` so a small stream trips the gate.

all:
	rm -f *.err
	: > obs.txt
	morloc make -o prog main.loc 2> build.err
	./prog mkStream stream.packet 2>> obs.err
	./prog mkData   data.packet   2>> obs.err

	# === Size gate: -f json on stream (buffered path) ===
	printf 'size gate -f json exit: ' >> obs.txt
	MORLOC_VIEW_MAX_BUFFER_BYTES=8 $(NEXUS) view stream.packet -f json > /dev/null 2>tmp.err; \
	  printf '%s\n' "$$?" >> obs.txt
	printf 'size gate -f json msg: '  >> obs.txt
	grep -oE 'pass --force' tmp.err | head -n 1 >> obs.txt 2>> obs.err
	rm -f tmp.err
	# With --force it succeeds (exit 0). We do not diff the JSON body;
	# the exit code alone confirms the guardrail lifted.
	printf 'size gate -f json --force exit: ' >> obs.txt
	MORLOC_VIEW_MAX_BUFFER_BYTES=8 $(NEXUS) view stream.packet -f json --force > /dev/null 2>>obs.err; \
	  printf '%s\n' "$$?" >> obs.txt

	# === Size gate: stream -> data ===
	printf 'size gate -d exit: ' >> obs.txt
	MORLOC_VIEW_MAX_BUFFER_BYTES=8 $(NEXUS) view stream.packet -d -o /tmp/g-s2d.packet 2>tmp.err; \
	  printf '%s\n' "$$?" >> obs.txt
	printf 'size gate -d msg: '  >> obs.txt
	grep -oE 'STREAM -> DATA' tmp.err | head -n 1 >> obs.txt 2>> obs.err
	rm -f tmp.err /tmp/g-s2d.packet
	printf 'size gate -d --force exit: ' >> obs.txt
	MORLOC_VIEW_MAX_BUFFER_BYTES=8 $(NEXUS) view stream.packet -d -o /tmp/g-s2d.packet --force 2>>obs.err; \
	  printf '%s\n' "$$?" >> obs.txt
	rm -f /tmp/g-s2d.packet

	# === Binary-to-tty refusal ===
	# `script -e -qc CMD /dev/null` allocates a pty for CMD's stdout
	# so our isatty() check fires; `-e` propagates the CMD's exit
	# status. `script` forwards CMD's stdout+stderr through the pty
	# to its own stdout, so we grep on stdout, not stderr.
	printf 'tty -f voidstar exit: ' >> obs.txt
	script -e -qc '$(NEXUS) view data.packet -f voidstar' /dev/null > tmp.out; \
	  printf '%s\n' "$$?" >> obs.txt
	printf 'tty -f voidstar msg: '  >> obs.txt
	grep -oE 'refusing to write binary' tmp.out | head -n 1 >> obs.txt 2>> obs.err
	rm -f tmp.out
	printf 'tty -f voidstar --force exit: ' >> obs.txt
	script -e -qc '$(NEXUS) view data.packet -f voidstar --force' /dev/null > /dev/null 2>>obs.err; \
	  printf '%s\n' "$$?" >> obs.txt

	# === CLI mutex / -f-vs-packet-flag / -p-on-non-packet ===
	printf 'mutex -s -d exit: ' >> obs.txt
	$(NEXUS) view data.packet -s -d > /dev/null 2>>obs.err; printf '%s\n' "$$?" >> obs.txt
	printf 'mutex -s -f json exit: ' >> obs.txt
	$(NEXUS) view data.packet -s -f json > /dev/null 2>tmp.err; printf '%s\n' "$$?" >> obs.txt
	printf 'mutex -s -f json msg: '  >> obs.txt
	grep -oE 'packet-type flag requires' tmp.err | head -n 1 >> obs.txt 2>> obs.err
	rm -f tmp.err
	# Prepare a non-packet input for the -p check.
	printf '["a","b"]' > raw.json
	printf 'mutex -p raw json exit: ' >> obs.txt
	$(NEXUS) view raw.json -p --schema as > /dev/null 2>tmp.err; printf '%s\n' "$$?" >> obs.txt
	printf 'mutex -p raw json msg: '  >> obs.txt
	grep -oE 'preserve-packet requires' tmp.err | head -n 1 >> obs.txt 2>> obs.err
	rm -f tmp.err

clean:
	rm -rf prog pools __pycache__ *.err
	rm -f stream.packet data.packet raw.json
