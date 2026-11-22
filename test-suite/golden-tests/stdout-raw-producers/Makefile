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

clean:
	rm -rf nexus pools __pycache__ *.err *.out *.pkt
	rm -rf *-build *build.tmp*
