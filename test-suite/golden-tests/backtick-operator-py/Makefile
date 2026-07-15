all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	grep -q 'and ' pools/main/pool.py 2>> obs.err && echo "pool contains 'and'" >> obs.txt
	grep -q 'or ' pools/main/pool.py 2>> obs.err && echo "pool contains 'or'" >> obs.txt
	./nexus test1 2>> obs.err >> obs.txt
	./nexus test2 2>> obs.err >> obs.txt
	./nexus test3 2>> obs.err >> obs.txt
	./nexus test4 2>> obs.err >> obs.txt

clean:
	rm -rf nexus pools
