all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	echo '"a"' > a.json
	echo '"b"' > b.json
	>obs.txt
	./nexus.py foo a.json b.json >> obs.txt
	./nexus.py foo '"a"' b.json >> obs.txt
	./nexus.py foo '"a"' '"b"' >> obs.txt
	./nexus.py foo '"a"' <(echo '"b"') >> obs.txt
	echo '"b"' | ./nexus.py foo '"a"' /dev/stdin >> obs.txt

clean:
	rm -rf nexus* pool* a.json b.json
