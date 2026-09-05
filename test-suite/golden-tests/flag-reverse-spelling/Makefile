all:
	rm -f *.err obs.txt
	morloc make -o t main.loc 2> build.err
	printf -- '--- opts help ---\n' > obs.txt
	./t opts -h 2>> obs.err | sed -n '/Optional/,/^$$/p' >> obs.txt
	printf -- '--- shortOnly help ---\n' >> obs.txt
	./t shortOnly -h 2>> obs.err | sed -n '/Optional/,/^$$/p' >> obs.txt
	printf -- '--- longOnly help ---\n' >> obs.txt
	./t longOnly -h 2>> obs.err | sed -n '/Optional/,/^$$/p' >> obs.txt
	printf -- '--- opts dispatch ---\n' >> obs.txt
	./t opts >> obs.txt 2>> obs.err
	./t opts -v >> obs.txt 2>> obs.err
	./t opts -q >> obs.txt 2>> obs.err
	./t opts --quiet >> obs.txt 2>> obs.err
	./t opts -v -q >> obs.txt 2>> obs.err
	./t opts -q -v >> obs.txt 2>> obs.err
	printf -- '--- shortOnly dispatch ---\n' >> obs.txt
	./t shortOnly -v >> obs.txt 2>> obs.err
	./t shortOnly -q >> obs.txt 2>> obs.err
	printf -- '--- longOnly dispatch ---\n' >> obs.txt
	./t longOnly -v >> obs.txt 2>> obs.err
	./t longOnly --quiet >> obs.txt 2>> obs.err
	printf -- '--- json-help ---\n' >> obs.txt
	./t --json-help 2>> obs.err | python3 describe.py >> obs.txt

clean:
	rm -rf t pools log __pycache__ *.err
	rm -rf *-build *build.tmp*
