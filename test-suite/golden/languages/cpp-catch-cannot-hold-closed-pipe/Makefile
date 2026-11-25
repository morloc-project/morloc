all:
	rm -f *.err got.txt rc.txt obs.txt
	morloc make -o nexus main.loc 2> build.err
	{ ./nexus -f jsonl @ run 2> obs.err ; echo $$? > rc.txt ; } | head -n 5 > got.txt
	printf 'head count: ' >> obs.txt ; wc -l < got.txt | tr -d ' ' >> obs.txt
	printf 'status: ' >> obs.txt ; cat rc.txt >> obs.txt
	printf 'caught: ' >> obs.txt ; grep -c CAUGHT obs.err >> obs.txt || true
	rm -f got.txt rc.txt

clean:
	rm -rf nexus *.err got.txt rc.txt *-build *build.tmp*
