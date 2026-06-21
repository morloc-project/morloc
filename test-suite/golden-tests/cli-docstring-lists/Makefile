# Per-element `list.source: file` + `list.form: packet` coverage:
#   * generate per-element packets via `nexus -f packet`
#   * write the packet paths into a manifest file
#   * consume the manifest with `form: list + list.source: file +
#     list.form: packet` (and the per-element auto path for one
#     comparison)
#   * verify the per-element `list.form: bytes` path on raw binary
#     files
#
# Probes for end-to-end correctness:
#   * 5 Int packets, 50 Int packets (stress)
#   * 1 Real packet
#   * 3 Str packets
#   * 3 (Int, Str) tuple packets
#   * 3 raw byte files
#   * list.form: packet vs list.form: auto on the same input -- both
#     should produce the same sum

all:
	rm -rf *.err obs.txt packets int_paths.txt stress_paths.txt real_paths.txt str_paths.txt tup_paths.txt blob_paths.txt
	morloc make -o nexus main.loc 2> build.err
	mkdir -p packets
	# --- Int packets (1..5) ---
	for i in 1 2 3 4 5 ; do \
	  ./nexus -f packet mk_int $$i > packets/int_$$i.pkt 2>> obs.err ; \
	done
	ls packets/int_*.pkt | sort > int_paths.txt
	# --- 50-Int stress (sum = 1275) ---
	mkdir -p packets/stress
	for i in $$(seq 1 50) ; do \
	  ./nexus -f packet mk_int $$i > packets/stress/int_$$i.pkt 2>> obs.err ; \
	done
	ls packets/stress/int_*.pkt | sort -V > stress_paths.txt
	# --- Real packet ---
	./nexus -f packet mk_real 3.5 > packets/real_1.pkt 2>> obs.err
	./nexus -f packet mk_real 1.25 > packets/real_2.pkt 2>> obs.err
	ls packets/real_*.pkt | sort > real_paths.txt
	# --- Str packets ---
	./nexus -f packet mk_str alpha > packets/str_a.pkt 2>> obs.err
	./nexus -f packet mk_str beta  > packets/str_b.pkt 2>> obs.err
	./nexus -f packet mk_str gamma > packets/str_c.pkt 2>> obs.err
	ls packets/str_*.pkt | sort > str_paths.txt
	# --- Tuple packets ---
	./nexus -f packet mk_tup '[1,"one"]'   > packets/tup_1.pkt 2>> obs.err
	./nexus -f packet mk_tup '[2,"two"]'   > packets/tup_2.pkt 2>> obs.err
	./nexus -f packet mk_tup '[3,"three"]' > packets/tup_3.pkt 2>> obs.err
	ls packets/tup_*.pkt | sort > tup_paths.txt
	# --- Raw binary blobs (not packets) for list.form: bytes ---
	python3 -c "import sys; sys.stdout.buffer.write(bytes(range(10)))" > packets/blob_a.bin
	python3 -c "import sys; sys.stdout.buffer.write(bytes(range(20)))" > packets/blob_b.bin
	python3 -c "import sys; sys.stdout.buffer.write(bytes(range(30)))" > packets/blob_c.bin
	ls packets/blob_*.bin | sort > blob_paths.txt
	# --- Run consumers ---
	>obs.txt
	./nexus size_ints_packets int_paths.txt >> obs.txt 2>> obs.err
	./nexus sum_ints_packets  int_paths.txt >> obs.txt 2>> obs.err
	./nexus sum_ints_auto     int_paths.txt >> obs.txt 2>> obs.err
	./nexus sum_ints_packets  stress_paths.txt >> obs.txt 2>> obs.err
	./nexus sum_reals_packets real_paths.txt >> obs.txt 2>> obs.err
	./nexus size_strs_packets str_paths.txt >> obs.txt 2>> obs.err
	./nexus size_tups_packets tup_paths.txt >> obs.txt 2>> obs.err
	./nexus size_byte_files   blob_paths.txt >> obs.txt 2>> obs.err
	./nexus total_byte_files  blob_paths.txt >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ packets *paths.txt *.err
