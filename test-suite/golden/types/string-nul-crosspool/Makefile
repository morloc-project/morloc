all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	> obs.txt
	./nexus cleanR ab 2>> obs.err >> obs.txt
	./nexus noStrR ab 2>> obs.err >> obs.txt
	./nexus bareR ab > /dev/null 2> obs.bare.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo bare_rejected >> obs.txt
	./nexus listR ab > /dev/null 2> obs.list.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo list_rejected >> obs.txt
	./nexus recordR ab > /dev/null 2> obs.record.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo record_rejected >> obs.txt
	grep -qF 'R cannot represent an embedded NUL' obs.bare.err && echo morloc_named_it >> obs.txt || echo UNEXPECTED_MESSAGE >> obs.txt
	grep -qF 'at [1] (byte 2 of 5)' obs.list.err && echo list_path_located >> obs.txt || echo UNEXPECTED_LIST_PATH >> obs.txt
	grep -qF 'at .b (byte 2 of 5)' obs.record.err && echo record_path_located >> obs.txt || echo UNEXPECTED_RECORD_PATH >> obs.txt
	MORLOC_SKIP_NULL_CHECK=1 ./nexus bareR ab > /dev/null 2> obs.skip.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo still_rejected_by_r >> obs.txt
	grep -qF 'R cannot represent an embedded NUL' obs.skip.err && echo UNEXPECTED_GUARD >> obs.txt || echo guard_skipped >> obs.txt
	grep -c 'morloc_get_value(.*, FALSE)' nexus-build/pools/r/pool.R >> obs.txt
	grep -c 'morloc_get_value(.*, TRUE)' nexus-build/pools/r/pool.R >> obs.txt

clean:
	rm -rf nexus __pycache__
	rm -f *.err
	rm -rf *-build *build.tmp*
