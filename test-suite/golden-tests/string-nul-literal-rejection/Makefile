all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	> obs.txt
	./nexus len 2>> obs.err >> obs.txt
	./nexus pyNul 2>> obs.err >> obs.txt
	morloc make -o bad bad-direct.loc 2> obs.direct.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo direct_rejected >> obs.txt
	morloc make -o bad bad-list.loc 2> obs.list.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo list_rejected >> obs.txt
	morloc make -o bad bad-tuple.loc 2> obs.tuple.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo tuple_rejected >> obs.txt
	morloc make -o bad bad-record.loc 2> obs.record.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo record_rejected >> obs.txt
	grep -qc CallStack obs.direct.err && echo UNEXPECTED_CALLSTACK >> obs.txt || echo no_callstack >> obs.txt
	grep -qc 'cannot represent in its native string type' obs.direct.err && echo named_the_language >> obs.txt || echo UNEXPECTED_MESSAGE >> obs.txt

clean:
	rm -rf nexus bad __pycache__
	rm -f *.err
	rm -rf *-build *build.tmp*
