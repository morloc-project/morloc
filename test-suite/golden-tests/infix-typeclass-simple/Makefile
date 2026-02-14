all:
	rm -f obs.txt *.err
	morloc make main.loc 2> build.err
	./nexus test_add 5 > obs.txt 2> obs.err
	./nexus test_mul 5 2>> obs.err  >> obs.txt
	./nexus test_expr 2>> obs.err  >> obs.txt
	./nexus test_negate 2>> obs.err  >> obs.txt

clean:
	rm -f nexus* pool* log
