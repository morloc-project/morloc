# Cross-language data-transfer test (Python -> C++ -> R).
#
# Two binaries built from the same source, differing only in the
# inline/file threshold:
#   * nexus-zero   ( --inline-size 0  ): every cross-pool transfer is a file
#   * nexus-thresh ( --inline-size 1k ): inline below 1 KiB, file above
#
# Both use --no-shm to disable shared memory and --tmpdir to persist
# the file packets for inspection.
#
# The run.sh script exercises three data shapes (Str, [Int], [[Int]])
# and confirms (a) round-trips work for each shape across all three
# languages and (b) the inline-size threshold actually decides
# routing: small payloads inline (no files), large payloads go to
# disk.

all:
	rm -f *.err obs.txt
	rm -rf tmp_zero tmp_thresh
	morloc make --no-shm --inline-size 0  --tmpdir $$PWD/tmp_zero   -o nexus-zero   main.loc 2>> build.err
	morloc make --no-shm --inline-size 1k --tmpdir $$PWD/tmp_thresh -o nexus-thresh main.loc 2>> build.err
	bash run.sh > obs.txt 2> obs.err
	# DIAGNOSTIC (macOS): files are created but the expected string content isn't
	# found. Dump what's actually on disk so we can see where/how macOS serializes.
	@echo "=== DIAG: tmp_zero listing ===" >> obs.err; ls -la tmp_zero >> obs.err 2>&1 || true
	@echo "=== DIAG: all files under tmp_zero + tmp_thresh ===" >> obs.err; find tmp_zero tmp_thresh -type f 2>/dev/null >> obs.err || true
	@echo "=== DIAG: od -c of first tmp_zero packet ===" >> obs.err; f=$$(ls tmp_zero/morloc-pkt-*.mpk 2>/dev/null | head -1); if [ -n "$$f" ]; then od -c "$$f" | head -40 >> obs.err; else echo "(no morloc-pkt-*.mpk in tmp_zero)" >> obs.err; fi
	@echo "=== DIAG: HELLO_MARKER anywhere under tmp_zero? ===" >> obs.err; grep -rlF HELLO_MARKER tmp_zero >> obs.err 2>&1 || echo "(HELLO_MARKER not found in tmp_zero)" >> obs.err

clean:
	rm -rf nexus-zero nexus-thresh pools tmp_zero tmp_thresh __pycache__
