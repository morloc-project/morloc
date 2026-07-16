# Positive coverage for the v1 CLI docstring grammar:
#   * Str: default inline, source: file, check.path: r
#   * [U8]: default JSON, form: bytes, bytes-only, source: inline + bytes
#   * Wider fixed-width arrays: form: bytes on [I32], [F64], [Bool]
#   * form: list on [Str], [Int], [(Int, Str)] (JSON & TSV fallback)
#   * Per-element: list.check.path: r
#   * many: true on [Str] and [Int]
#   * Default compound (tuple) via inline JSON
#
# Each call writes one line to obs.txt; exp.txt holds the expected
# values in order.

all:
	rm -f *.err obs.txt *.bin *.tmp text.txt strs.txt ints.txt tuples_json.txt tuples_tsv.txt paths.txt
	morloc make -o nexus main.loc 2> build.err
	# --- Generate input files ---
	printf 'Hello, World!' > text.txt
	python3 -c "import sys; sys.stdout.buffer.write(bytes([1,2,3,4,5]))" > u8.bin
	python3 -c "import struct,sys; sys.stdout.buffer.write(b''.join(struct.pack('<i',i) for i in [10,20,30]))" > i32.bin
	python3 -c "import struct,sys; sys.stdout.buffer.write(b''.join(struct.pack('<d',f) for f in [1.5,2.5]))" > f64.bin
	python3 -c "import sys; sys.stdout.buffer.write(bytes([0,1,0,1]))" > bool.bin
	printf 'apple\nbanana\ncherry\n' > strs.txt
	printf '1\n2\n3\n4\n5\n' > ints.txt
	printf '[1,"one"]\n[2,"two"]\n[3,"three"]\n' > tuples_json.txt
	printf '1\tone\n2\ttwo\n3\tthree\n' > tuples_tsv.txt
	printf 'text.txt\nu8.bin\ni32.bin\n' > paths.txt
	# --- Run all positive cases ---
	>obs.txt
	./nexus str_default 'hello world' >> obs.txt 2>> obs.err
	./nexus str_file text.txt >> obs.txt 2>> obs.err
	./nexus str_check_r main.loc >> obs.txt 2>> obs.err
	./nexus bytes_default '[1,2,3,4,5]' >> obs.txt 2>> obs.err
	./nexus bytes_hybrid u8.bin >> obs.txt 2>> obs.err
	./nexus bytes_only u8.bin >> obs.txt 2>> obs.err
	./nexus bytes_inline GATTACA >> obs.txt 2>> obs.err
	./nexus bytes_int32 i32.bin >> obs.txt 2>> obs.err
	./nexus bytes_float64 f64.bin >> obs.txt 2>> obs.err
	./nexus bytes_bool bool.bin >> obs.txt 2>> obs.err
	./nexus list_str strs.txt >> obs.txt 2>> obs.err
	./nexus list_int ints.txt >> obs.txt 2>> obs.err
	./nexus list_tuple_json tuples_json.txt >> obs.txt 2>> obs.err
	./nexus list_tuple_tsv tuples_tsv.txt >> obs.txt 2>> obs.err
	./nexus list_path_check paths.txt >> obs.txt 2>> obs.err
	./nexus many_str a b c d >> obs.txt 2>> obs.err
	./nexus many_int 10 20 30 40 >> obs.txt 2>> obs.err
	./nexus default_tuple '[42,"hi"]' >> obs.txt 2>> obs.err
	# --- form: list on unrolled record entries ---
	./nexus record_list_str --xs strs.txt >> obs.txt 2>> obs.err
	./nexus record_list_str --xs '["one","two"]' >> obs.txt 2>> obs.err
	./nexus record_list_int --ys ints.txt >> obs.txt 2>> obs.err
	./nexus record_list_int --ys '[100,200,300]' >> obs.txt 2>> obs.err
	# --- source: inline + form: bytes shortcut on a record entry ---
	./nexus record_dna_len --dna GATTACA >> obs.txt 2>> obs.err
	# --- check.path: r on a record entry (positive + negative) ---
	./nexus record_pathr_len --pathr strs.txt >> obs.txt 2>> obs.err
	./nexus record_pathr_len --pathr no-such-file 2>&1 | grep -c "check.path" >> obs.txt || true
	# --- Unix `-` shorthand on a record entry with check.path: r ---
	./nexus record_pathr_len --pathr - >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.bin text.txt strs.txt ints.txt tuples_json.txt tuples_tsv.txt paths.txt
