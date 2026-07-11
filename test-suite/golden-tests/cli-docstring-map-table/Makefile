# `Map a b` (packed to `[(a, b)]` at the wire) can be read from a
# two-column table via `form: list`, because CLI schema classification
# runs on the packed wire schema, not the source-level general type.
# Covers TSV fallback, JSON-array-per-line, key-position round-trip,
# and value sum.

all:
	rm -f *.err *.tmp map_tsv.txt map_json.txt
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	# --- Generate input files ---
	printf 'a\t1\nb\t2\nc\t3\n' > map_tsv.txt
	printf '["a",1]\n["b",2]\n["c",3]\n' > map_json.txt
	# --- Positive cases: Map read from a two-column table ---
	./nexus map_size_tsv map_tsv.txt >> obs.txt 2>> obs.err
	./nexus map_size_json map_json.txt >> obs.txt 2>> obs.err
	./nexus map_values_sum map_tsv.txt >> obs.txt 2>> obs.err
	./nexus map_keys_joined map_tsv.txt >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ log map_tsv.txt map_json.txt
