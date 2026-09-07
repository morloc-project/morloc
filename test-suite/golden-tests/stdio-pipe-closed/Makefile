all:
	rm -f *.err got.txt gotc.txt rc.txt rcc.txt obs.txt
	morloc make -o nexus main.loc 2> build.err
	{ ./nexus -f jsonl @ produceBig 2> obs.err ; echo $$? > rc.txt ; } | head -n 5 > got.txt
	printf 'head count: ' >> obs.txt ; wc -l < got.txt | tr -d ' ' >> obs.txt
	printf 'head first: ' >> obs.txt ; head -n 1 got.txt >> obs.txt
	printf 'head last: '  >> obs.txt ; tail -n 1 got.txt >> obs.txt
	printf 'head status: ' >> obs.txt ; cat rc.txt >> obs.txt
	printf 'clean: ' >> obs.txt ; if grep -qiE "already open|parse failed|panic|INTERNAL|Traceback" obs.err ; then echo FAIL >> obs.txt ; else echo PASS >> obs.txt ; fi
	{ ./nexus -f jsonl @ produceInCatch 2>> obs.err ; echo $$? > rcc.txt ; } | head -n 5 > gotc.txt
	printf 'catch count: ' >> obs.txt ; wc -l < gotc.txt | tr -d ' ' >> obs.txt
	printf 'catch first: ' >> obs.txt ; head -n 1 gotc.txt >> obs.txt
	printf 'catch status: ' >> obs.txt ; cat rcc.txt >> obs.txt
	printf 'full count: ' >> obs.txt ; ./nexus -f jsonl @ produceBig 2>> obs.err | wc -l | tr -d ' ' >> obs.txt
	rm -f got.txt gotc.txt rc.txt rcc.txt

clean:
	rm -rf nexus __pycache__ *.err got.txt gotc.txt rc.txt rcc.txt *-build *build.tmp*
