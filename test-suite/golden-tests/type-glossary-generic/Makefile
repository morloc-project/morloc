all:
	rm -f *.err obs.txt
	morloc make -o main main.loc 2> build.err
	printf -- '--- json-help ---\n' > obs.txt
	./main --json-help 2>> obs.err | python3 describe.py >> obs.txt
	printf -- '--- mkStr help ---\n' >> obs.txt
	./main mkStr -h >> obs.txt 2>> obs.err
	printf -- '--- mkOptStr help ---\n' >> obs.txt
	./main mkOptStr -h >> obs.txt 2>> obs.err
	printf -- '--- viaCtor help ---\n' >> obs.txt
	./main viaCtor -h >> obs.txt 2>> obs.err
	printf -- '--- run ---\n' >> obs.txt
	./main mkStr hello >> obs.txt 2>> obs.err
	./main mkWrap 3 >> obs.txt 2>> obs.err
	./main holder x >> obs.txt 2>> obs.err
	./main mkQ 1 y >> obs.txt 2>> obs.err

clean:
	rm -rf main pools log __pycache__ *.err
	rm -rf *-build *build.tmp*
