all:
	: > obs.txt
	rm -f *.err f.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus -f packet produce 5 > f.dat 2>> obs.err
	# --- IStream: file, stdin (omitted), stdin (dash) all read the same batch ---
	printf -- '--- readStream FILE ---\n' >> obs.txt
	./nexus readStream f.dat >> obs.txt 2>> obs.err
	printf -- '--- readStream STDIN omitted ---\n' >> obs.txt
	./nexus -f packet produce 5 | ./nexus readStream >> obs.txt 2>> obs.err
	printf -- '--- readStream STDIN dash ---\n' >> obs.txt
	./nexus -f packet produce 5 | ./nexus readStream - >> obs.txt 2>> obs.err
	# --- IFile fast path on a real file; IStream fallback on stdin ---
	printf -- '--- readAny FILE (IFile count) ---\n' >> obs.txt
	./nexus readAny f.dat >> obs.txt 2>> obs.err
	printf -- '--- readAny STDIN (IFile fails, IStream fallback) ---\n' >> obs.txt
	./nexus -f packet produce 5 | ./nexus readAny >> obs.txt 2>> obs.err
	# --- schema guard: an [Int] packet fed to an IStream Str reader is rejected ---
	printf -- '--- schema guard ([Int] pkt -> IStream Str) ---\n' >> obs.txt
	if ./nexus -f packet produce 5 | ./nexus readStr >> obs.txt 2>> obs.err; then \
		echo "UNEXPECTED: schema mismatch accepted" >> obs.txt; \
	else \
		echo "OK: schema mismatch rejected" >> obs.txt; \
	fi
	# --- foreign (non-morloc) stdin is rejected with a clean error ---
	printf -- '--- foreign stdin (>=32 bytes of text) ---\n' >> obs.txt
	if printf 'this is definitely not a morloc packet, just plain text bytes\n' | ./nexus readStream >> obs.txt 2>> obs.err; then \
		echo "UNEXPECTED: foreign input accepted" >> obs.txt; \
	else \
		echo "OK: foreign input rejected" >> obs.txt; \
	fi
	# --- truncated (<32 byte) stdin is rejected, not silently empty ---
	printf -- '--- truncated stdin (8 bytes) ---\n' >> obs.txt
	if printf '12345678' | ./nexus readStream >> obs.txt 2>> obs.err; then \
		echo "UNEXPECTED: truncated input accepted" >> obs.txt; \
	else \
		echo "OK: truncated input rejected" >> obs.txt; \
	fi
	# --- empty stdin is a legitimate empty batch ---
	printf -- '--- empty stdin ---\n' >> obs.txt
	printf '' | ./nexus readStream >> obs.txt 2>> obs.err
	# --- compile-time rejection: two `stdin: true` positionals ---
	printf -- '--- compile rejection: two stdin args ---\n' >> obs.txt
	if morloc make -o bad_two bad_two.loc 2>> obs.err; then \
		echo "UNEXPECTED: two stdin args accepted" >> obs.txt; \
	else \
		echo "OK: two stdin args rejected" >> obs.txt; \
	fi

clean:
	rm -rf nexus bad_two pools __pycache__ *.err f.dat
