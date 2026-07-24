all:
	: > obs.txt
	rm -f *.err lines.txt
	printf 'alpha\nbeta\ngamma\n' > lines.txt
	morloc make -o nexus main.loc 2> build.err
	# --- fhead: default (JSON), each terminal flag, then again with long-form ---
	printf -- '--- fhead default ---\n' >> obs.txt
	./nexus fhead lines.txt >> obs.txt 2>> obs.err
	printf -- '--- fhead -l ---\n' >> obs.txt
	./nexus fhead -l lines.txt >> obs.txt 2>> obs.err
	printf -- '--- fhead --lines ---\n' >> obs.txt
	./nexus fhead --lines lines.txt >> obs.txt 2>> obs.err
	printf -- '--- fhead -c ---\n' >> obs.txt
	./nexus fhead -c lines.txt >> obs.txt 2>> obs.err
	printf -- '--- fhead -s (effectful reformatter, B=Str) ---\n' >> obs.txt
	./nexus fhead -s lines.txt >> obs.txt 2>> obs.err
	printf -- '--- fhead -p (pure reformatter, B=Str, no <IO> on terminal) ---\n' >> obs.txt
	./nexus fhead -p lines.txt >> obs.txt 2>> obs.err
	printf -- '--- fhead --plain (long form) ---\n' >> obs.txt
	./nexus fhead --plain lines.txt >> obs.txt 2>> obs.err
	# --- -f composes with terminal, does not conflict ---
	printf -- '--- -f json + -s (re-encode reformatter Str) ---\n' >> obs.txt
	./nexus -f json fhead -s lines.txt >> obs.txt 2>> obs.err
	printf -- '--- -f jsonl + -p (pure reformatter through jsonl) ---\n' >> obs.txt
	./nexus -f jsonl fhead -p lines.txt >> obs.txt 2>> obs.err
	printf -- '--- -f json + -l (sink writes stdout; JSON of () harmless) ---\n' >> obs.txt
	./nexus -f json fhead -l lines.txt >> obs.txt 2>> obs.err
	# --- describe: default (Str), then via its --say sink ---
	printf -- '--- describe default ---\n' >> obs.txt
	./nexus describe 42 >> obs.txt 2>> obs.err
	printf -- '--- describe --say ---\n' >> obs.txt
	./nexus describe --say 42 >> obs.txt 2>> obs.err
	# --- Help output: presence + descriptions ---
	printf -- '--- fhead --help lines (grep terminal flags + descriptions) ---\n' >> obs.txt
	./nexus fhead --help 2>> obs.err | grep -E '(-l, --lines|-c, --csv|-s, --sum|-p, --plain|Print each string|Join all|Pure semicolon|comma-separated)' >> obs.txt
	printf -- '--- describe --help lines (grep --say) ---\n' >> obs.txt
	./nexus describe --help 2>> obs.err | grep -E '(--say|Human-readable)' >> obs.txt
	# --- Return block lists the return type per formatter flag ---
	printf -- '--- fhead --help Return block (per-formatter return types) ---\n' >> obs.txt
	./nexus fhead --help 2>> obs.err | awk '/^Return:/{f=1} f&&/^$$/{exit} f{print}' >> obs.txt
	printf -- '--- describe --help Return block (single terminal) ---\n' >> obs.txt
	./nexus describe --help 2>> obs.err | awk '/^Return:/{f=1} f&&/^$$/{exit} f{print}' >> obs.txt
	# --- Sibling mutual exclusion: -l -c must error (nonzero exit) ---
	printf -- '--- fhead -l -c (should error) ---\n' >> obs.txt
	if ./nexus fhead -l -c lines.txt >> obs.txt 2>> obs.err; then \
		echo "UNEXPECTED: fhead -l -c returned success" >> obs.txt; \
	else \
		echo "OK: sibling terminals rejected" >> obs.txt; \
	fi
	# --- Internal entry (mlcp_*) must not be user-invokable ---
	printf -- '--- ./nexus mlcp_fhead_lines (should error) ---\n' >> obs.txt
	if ./nexus mlcp_fhead_lines lines.txt >> obs.txt 2>> obs.err; then \
		echo "UNEXPECTED: internal entry accepted by name" >> obs.txt; \
	else \
		echo "OK: internal entry hidden" >> obs.txt; \
	fi
	# --- Top-level --help must NOT list mlcp_* entries ---
	printf -- '--- top-level --help mlcp match count (expect 0) ---\n' >> obs.txt
	./nexus --help 2>> obs.err | grep -c 'mlcp_' >> obs.txt; true

clean:
	rm -rf nexus pools __pycache__ lines.txt
