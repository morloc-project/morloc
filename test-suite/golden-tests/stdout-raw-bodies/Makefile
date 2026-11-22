all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus texts 1 > text.out 2>> obs.err
	python3 expected.py text | cmp -s - text.out && echo TEXT-SAME >> obs.txt || echo TEXT-DIFFERS >> obs.txt
	./nexus binary 1 > bytes.out 2>> obs.err
	python3 expected.py bytes | cmp -s - bytes.out && echo BYTES-SAME >> obs.txt || echo BYTES-DIFFERS >> obs.txt
	sh -c './nexus texts 1 2>> obs.err; echo $$? >> obs.txt' | head -c 5 > /dev/null
	./nexus -z 3 texts 1 > textz.out 2>> obs.err
	python3 expected.py text | cmp -s - textz.out && echo TEXT-Z-SAME >> obs.txt || echo TEXT-Z-DIFFERS >> obs.txt
	./nexus bad 1 > bad.out 2> bad.err && echo UNEXPECTED >> obs.txt || echo OK >> obs.txt
	grep -q "C++.*not valid UTF-8" bad.err && echo NAMED >> obs.txt || echo UNNAMED >> obs.txt
	MORLOC_SKIP_UTF8_CHECK=1 ./nexus bad 1 > skip.out 2> skip.err && echo OK >> obs.txt || echo UNEXPECTED >> obs.txt
	printf 'a\377b\na\377b\na\377b\n' | cmp -s - skip.out && echo SKIP-SAME >> obs.txt || echo SKIP-DIFFERS >> obs.txt
	for f in json jsonl; do \
	  ./nexus -f $$f badStrs 1 > $$f.out 2> $$f.err && echo UNEXPECTED >> obs.txt || echo OK >> obs.txt; \
	  grep -q "C++.*not valid UTF-8 (byte 1 of 3)" $$f.err && echo NAMED >> obs.txt || echo UNNAMED >> obs.txt; \
	  MORLOC_SKIP_UTF8_CHECK=1 ./nexus -f $$f badStrs 1 > skip$$f.out 2> skip$$f.err && echo UNEXPECTED >> obs.txt || echo OK >> obs.txt; \
	  grep -q "not valid UTF-8 (byte 1 of 3)" skip$$f.err && ! grep -q "outside" skip$$f.err && echo NAMED >> obs.txt || echo UNNAMED >> obs.txt; \
	done

clean:
	rm -rf nexus pools __pycache__ *.err *.out
	rm -rf *-build *build.tmp*
