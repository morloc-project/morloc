all:
	rm -f obs.txt
	morloc make main.loc > /dev/null 2> build.err
	echo foo > obs.txt 2> obs.err
	./nexus foo --alg-config=algconf.json [1,2,3] 2>> obs.err  >> obs.txt
	./nexus foo --alg-config=algconf.json -m 0 -n 0 [1,2,3] 2>> obs.err  >> obs.txt
	./nexus foo -m 0 -n 0 [1,2,3] 2>> obs.err  >> obs.txt
	./nexus foo -m 5 -n 0 [1,2,3] 2>> obs.err  >> obs.txt
	./nexus foo -m 5 -n 2 [1,2,3] 2>> obs.err  >> obs.txt
	./nexus foo -n 2 -m 5 [1,2,3] 2>> obs.err  >> obs.txt
	./nexus foo -m 5 [1,2,3] 2>> obs.err  >> obs.txt
	./nexus foo [1,2,3] 2>> obs.err  >> obs.txt
	echo bar 2>> obs.err  >> obs.txt
	./nexus bar [1,2,3] 2>> obs.err  >> obs.txt
	./nexus bar -y a [1,2,3] 2>> obs.err  >> obs.txt
	./nexus bar -y a -t 5 -m 6 [1,2,3] 2>> obs.err  >> obs.txt
	./nexus bar -y f -t 5 -m 6 --alg-conf=algconf.json [1,2,3] 2>> obs.err  >> obs.txt
	echo travelTime 2>> obs.err  >> obs.txt
	./nexus travelTime -w 2 3 4 2>> obs.err  >> obs.txt
	echo baz 2>> obs.err  >> obs.txt
	./nexus baz foo 2>> obs.err  >> obs.txt
	./nexus baz "foo bar" 2>> obs.err  >> obs.txt
	echo bif 2>> obs.err  >> obs.txt
	./nexus bif 2>> obs.err  >> obs.txt
	./nexus bif --clean 2>> obs.err  >> obs.txt
	./nexus bif --sys-config sysconf.json --clean 2>> obs.err  >> obs.txt
	./nexus bif --sys-config sysconf.json 2>> obs.err  >> obs.txt
	./nexus bif --sys-config sysconf.json --no-clean 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus* __pycache__ pool*
