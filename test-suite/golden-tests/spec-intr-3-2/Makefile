all:
	: > obs.txt
	rm -f *.err z.json n.txt q.txt v.mpk w.mpk
	morloc make -o nexus main.loc 2> build.err
	./nexus roundTrip z.json >> obs.txt 2>> obs.err
	printf 'null\n' > n.txt
	./nexus wrap n.txt >> obs.txt 2>> obs.err
	printf '"null"\n' > q.txt
	./nexus wrap q.txt >> obs.txt 2>> obs.err
	./nexus -f mpk -o v.mpk ints 2>> obs.err
	./nexus loadInts v.mpk >> obs.txt 2>> obs.err
	./nexus -f mpk -o w.mpk int49 2>> obs.err
	./nexus loadInt w.mpk >> obs.txt 2>> obs.err
	rm -f z.json n.txt q.txt v.mpk w.mpk

clean:
	rm -f *.err z.json n.txt q.txt v.mpk w.mpk nexus
	rm -rf *-build *build.tmp*
