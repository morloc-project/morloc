all:
	rm -f *.err *.out *.pkt obs.txt
	morloc make -o nexus main.loc 2> build.err
	for c in cppBad cppBlob pyBad pyBadArray pySurrogate rBadVec rBadList rBadRaw rBytes rToPy pyRendered rRendered; do \
	  ./nexus $$c 1 > $$c.out 2> $$c.err && echo "$$c UNEXPECTED" >> obs.txt || echo "$$c FAILED" >> obs.txt; \
	  grep -o "[A-Za-z+]* produced a Str that is not valid UTF-8[^:]*" $$c.err | head -n 1 >> obs.txt; \
	  od -An -tx1 $$c.out | grep -q ff && echo "$$c LEAKED" >> obs.txt || true; \
	done
	for c in stagedCpp stagedR; do \
	  ./nexus $$c 1 -a -b=$$c-b.out > $$c.out 2> $$c.err && echo "$$c UNEXPECTED" >> obs.txt || echo "$$c FAILED" >> obs.txt; \
	  grep -o "[A-Za-z+]* produced a Str that is not valid UTF-8[^:]*" $$c.err | head -n 1 >> obs.txt; \
	  od -An -tx1 $$c.out | grep -q ff && echo "$$c LEAKED" >> obs.txt || true; \
	done
	./nexus -f jsonl rLatin 1 >> obs.txt 2>> obs.err || echo "rLatin FAILED" >> obs.txt
	for c in pyRendered rRendered; do \
	  MORLOC_SKIP_UTF8_CHECK=1 ./nexus $$c 1 > skip-$$c.out 2> skip-$$c.err && echo "$$c skipped OK" >> obs.txt || echo "$$c skipped FAILED" >> obs.txt; \
	  printf 'a\377b\n' | cmp -s - skip-$$c.out && echo "$$c SAME" >> obs.txt || echo "$$c DIFFERS" >> obs.txt; \
	done
	MORLOC_SKIP_UTF8_CHECK=1 ./nexus -f packet cppBad 1 > bad.pkt 2> pkt.err && echo "packet OK" >> obs.txt || echo "packet FAILED" >> obs.txt
	for c in fwd fwdCpp; do for skip in 0 1; do \
	  MORLOC_SKIP_UTF8_CHECK=$$skip ./nexus $$c < bad.pkt > $$c.out 2> $$c.err && echo "$$c $$skip UNEXPECTED" >> obs.txt || echo "$$c $$skip FAILED" >> obs.txt; \
	  grep -o "not valid UTF-8 (byte 1 of 4) in data read from outside" $$c.err | head -n 1 >> obs.txt; \
	  od -An -tx1 $$c.out | grep -q ff && echo "$$c LEAKED" >> obs.txt || true; \
	done; done
	for c in "stagedCpp 1 -a" "stagedCpp 1 -c" "stagedCpp 1 -a -c=skip-cat.out" "stagedR 1 -a" "stagedValue 1 -a -b=skip-both.out"; do \
	  MORLOC_SKIP_UTF8_CHECK=1 ./nexus $$c > skip-staged.out 2> skip-staged.err && echo "$$c skipped OK" >> obs.txt || echo "$$c skipped FAILED" >> obs.txt; \
	  printf 'ok\na\377b\n' | cmp -s - skip-staged.out && echo "$$c SAME" >> obs.txt || echo "$$c DIFFERS" >> obs.txt; \
	done
	printf 'ok\na\377b\n' | cmp -s - skip-cat.out && echo "cat file SAME" >> obs.txt || echo "cat file DIFFERS" >> obs.txt
	printf 'ok\na\377b\n' | cmp -s - skip-both.out && echo "both file SAME" >> obs.txt || echo "both file DIFFERS" >> obs.txt
	rm -rf forged && mkdir -p forged/morloc-stage-arg001 forged/morloc-stage-open01 forged/morloc-stage-link01 forged/morloc-stage-good01
	chmod 700 forged/morloc-stage-arg001 forged/morloc-stage-link01 forged/morloc-stage-good01 && chmod 755 forged/morloc-stage-open01
	for d in arg001 open01 good01; do cp bad.pkt forged/morloc-stage-$$d/stream.pkt && chmod 600 forged/morloc-stage-$$d/stream.pkt; done
	ln -s ../morloc-stage-good01/stream.pkt forged/morloc-stage-link01/stream.pkt
	MORLOC_TMPDIR=$(CURDIR)/forged MORLOC_SKIP_UTF8_CHECK=1 ./nexus fwdArg $(CURDIR)/forged/morloc-stage-arg001/stream.pkt > fwdArg.out 2> fwdArg.err && echo "fwdArg UNEXPECTED" >> obs.txt || echo "fwdArg FAILED" >> obs.txt
	grep -o "not valid UTF-8 (byte 1 of 4) in data read from outside" fwdArg.err | head -n 1 >> obs.txt
	for d in open01 link01 good01; do \
	  MORLOC_TMPDIR=$(CURDIR)/forged MORLOC_SKIP_UTF8_CHECK=1 morloc-nexus run --mlc-internal mlcr_stagedCpp_all --mlc-input $(CURDIR)/forged/morloc-stage-$$d/stream.pkt nexus-build/manifest.json > replay.out 2> replay.err && echo "replay $$d OK" >> obs.txt || echo "replay $$d FAILED" >> obs.txt; \
	  grep -o "not valid UTF-8 (byte 1 of 4) in data read from outside" replay.err | head -n 1 >> obs.txt; \
	  printf 'ok\na\377b\n' | cmp -s - replay.out && echo "replay $$d SAME" >> obs.txt || true; \
	done

clean:
	rm -rf nexus pools __pycache__ *.err *.out *.pkt forged
	rm -rf *-build *build.tmp*
