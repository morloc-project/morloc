all:
	rm -f obs.txt *.err
	morloc make main.loc 2> build.err
	./nexus test_arithmetic       > obs.txt 2> obs.err
	./nexus test_precedence       2>> obs.err  >> obs.txt
	./nexus test_exponentiation   2>> obs.err  >> obs.txt
	./nexus test_append           2>> obs.err  >> obs.txt
	./nexus test_application      2>> obs.err  >> obs.txt

clean:
	rm -rf pool* nexus* __pycache__
