W := MORLOC_WRITE_BUFFER_BYTES=65536

define run_lang
	echo "--- $(1) ---" >> obs.txt
	rm -f log.txt *.out
	$(W) ./$(1) stream log.txt --all >> obs.txt 2>> obs.err
	$(W) ./$(1) stream log.txt --count >> obs.txt 2>> obs.err
	$(W) ./$(1) stream log.txt --batches >> obs.txt 2>> obs.err
	$(W) ./$(1) stream log.txt --folded >> obs.txt 2>> obs.err
	$(W) ./$(1) stream log.txt --marks >> obs.txt 2>> obs.err
	$(W) ./$(1) stream log.txt --marks=k.out --no-stdout >> obs.txt 2>> obs.err
	cat k.out >> obs.txt
	: > log.txt
	$(W) ./$(1) stream log.txt --all=a.out --count=n.out --batches=b.out --folded=f.out --no-stdout >> obs.txt 2>> obs.err
	cat log.txt a.out n.out b.out f.out >> obs.txt
	$(W) ./$(1) twoSites log.txt --all >> obs.txt 2>> obs.err
	$(W) ./$(1) twoSites log.txt --batches >> obs.txt 2>> obs.err
	$(W) ./$(1) twoSites log.txt --marks >> obs.txt 2>> obs.err
	$(W) ./$(1) twoSites log.txt --all=t.out --no-stdout >> obs.txt 2>> obs.err
	cat t.out >> obs.txt
	$(W) ./$(1) branch log.txt true --all >> obs.txt 2>> obs.err
	$(W) ./$(1) branch log.txt false --all >> obs.txt 2>> obs.err
endef

all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus-py main-py.loc 2> build-py.err
	morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err
	morloc make -o nexus-r main-r.loc 2> build-r.err
	morloc make -o nexus-rust main-rust.loc 2> build-rust.err
	$(call run_lang,nexus-py)
	$(call run_lang,nexus-cpp)
	$(call run_lang,nexus-r)
	$(call run_lang,nexus-rust)
	echo "--- serving ---" >> obs.txt
	./nexus-py twoSites -h 2>> obs.err | sed -n '/^Return:/,/^$$/p' >> obs.txt
	./nexus-py --mcp-tools | python3 -c 'import json,sys; [print(t["name"], t["inputSchema"]["properties"]["_render"]["enum"]) for t in json.load(sys.stdin)["tools"] if t["name"] in ("stream","twoSites")]' >> obs.txt
	./serve.sh >> obs.txt 2>> obs.err
	echo "--- formats ---" >> obs.txt
	: > log.txt
	if ./nexus-py -f mpk stream log.txt --all=m.out 2>> obs.err ; then echo UNEXPECTED >> obs.txt ; else echo OK >> obs.txt ; fi
	if ./nexus-py -f csv stream log.txt --count=c.out --no-stdout 2>> obs.err ; then echo UNEXPECTED >> obs.txt ; else echo OK >> obs.txt ; fi
	wc -l < log.txt | tr -d ' ' >> obs.txt
	./nexus-py -f jsonl stream log.txt --count=j.out --no-stdout 2>> obs.err
	cat j.out >> obs.txt

clean:
	rm -rf nexus-py nexus-cpp nexus-r nexus-rust pools __pycache__ *.err *.out log.txt port.txt
	rm -rf *-build *build.tmp*
