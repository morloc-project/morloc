all:
	morloc make main.loc > /dev/null
	echo foo > obs.txt
	./nexus foo --alg-config=algconf.json [1,2,3] >> obs.txt
	./nexus foo --alg-config=algconf.json -m 0 -n 0 [1,2,3] >> obs.txt
	./nexus foo -m 0 -n 0 [1,2,3] >> obs.txt
	./nexus foo -m 5 -n 0 [1,2,3] >> obs.txt
	./nexus foo -m 5 -n 2 [1,2,3] >> obs.txt
	./nexus foo -n 2 -m 5 [1,2,3] >> obs.txt
	./nexus foo -m 5 [1,2,3] >> obs.txt
	./nexus foo [1,2,3] >> obs.txt
	echo bar >> obs.txt
	./nexus bar [1,2,3] >> obs.txt
	./nexus bar -y '"a"' [1,2,3] >> obs.txt
	./nexus bar -y '"a"' -t 5 -m 6 [1,2,3] >> obs.txt
	./nexus bar -y '"f"' -t 5 -m 6 --alg-conf=algconf.json [1,2,3] >> obs.txt
	echo travelTime >> obs.txt
	./nexus travelTime -w 2 3 4 >> obs.txt
	echo baz >> obs.txt
	./nexus baz 5 >> obs.txt
	echo bif >> obs.txt
	./nexus bif >> obs.txt
	./nexus bif --clean >> obs.txt
	./nexus bif --sys-config sysconf.json --clean >> obs.txt
	./nexus bif --sys-config sysconf.json >> obs.txt
	./nexus bif --sys-config sysconf.json --no-clean >> obs.txt

clean:
	rm -rf nexus* __pycache__ pool*
