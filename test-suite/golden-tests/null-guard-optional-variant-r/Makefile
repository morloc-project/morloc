all:
	rm -f *.err obs.txt deep.json
	morloc make -o nexus main.loc 2> build.err
	> obs.txt
	./nexus cleanOptR ab 2>> obs.err >> obs.txt
	./nexus cleanTagR ab 2>> obs.err >> obs.txt
	./nexus optR ab > /dev/null 2> obs.opt.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo opt_rejected >> obs.txt
	grep -qF 'at (some) (byte 2 of 5)' obs.opt.err && echo opt_path_located >> obs.txt || echo UNEXPECTED_OPT_PATH >> obs.txt
	./nexus tagR ab > /dev/null 2> obs.tag.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo tag_rejected >> obs.txt
	grep -qF 'at .Named.0 (byte 2 of 5)' obs.tag.err && echo tag_path_located >> obs.txt || echo UNEXPECTED_TAG_PATH >> obs.txt
	./nexus chainR ab 100000 > /dev/null 2> obs.chain.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo chain_rejected >> obs.txt
	grep -qF 'segments..].next(some).next(some).next(some).next(some).next(some).next(some).next(some).next(some).label (byte 2 of 5)' obs.chain.err && echo chain_path_located >> obs.txt || echo UNEXPECTED_CHAIN_PATH >> obs.txt
	python3 gen.py 200 > deep.json
	./nexus deepArgR deep.json > /dev/null 2> obs.arg.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo arg_rejected >> obs.txt
	grep -qF 'segments..].next(some).next(some).next(some).next(some).next(some).next(some).next(some).next(some).label (byte 2 of 5)' obs.arg.err && echo arg_path_located >> obs.txt || echo UNEXPECTED_ARG_PATH >> obs.txt
	rm -f deep.json

clean:
	rm -rf nexus __pycache__ deep.json
	rm -f *.err
	rm -rf *-build *build.tmp*
