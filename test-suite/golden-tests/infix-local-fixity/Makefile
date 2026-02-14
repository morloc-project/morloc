all:
	rm -f obs.txt *.err
	morloc make main.loc 2> build.err
	./nexus test_a > obs.txt 2> obs.err
	./nexus test_b 2>> obs.err  >> obs.txt

clean:
	rm -f nexus* pool* log
