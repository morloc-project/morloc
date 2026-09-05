all:
	rm -f *.err obs.txt
	morloc make -o box main.loc 2> build.err
	printf -- '--- takeInt help ---\n' > obs.txt
	./box takeInt -h >> obs.txt 2>> obs.err
	printf -- '--- takeStr help ---\n' >> obs.txt
	./box takeStr -h >> obs.txt 2>> obs.err
	printf -- '--- json-help ---\n' >> obs.txt
	./box --json-help 2>> obs.err | python3 describe.py >> obs.txt
	printf -- '--- run ---\n' >> obs.txt
	./box takeInt '{"it":7}' >> obs.txt 2>> obs.err
	./box takeStr '{"it":"abcd"}' >> obs.txt 2>> obs.err

clean:
	rm -rf box pools log __pycache__ *.err
	rm -rf *-build *build.tmp*
