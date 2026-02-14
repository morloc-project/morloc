all:
	rm -f obs.txt *.err
	morloc make main.loc 2> build.err
	./nexus test_comp > obs.txt 2> obs.err
	./nexus test_min 2>> obs.err  >> obs.txt
	./nexus test_not_equal 2>> obs.err  >> obs.txt

clean:
	rm -f nexus* pool* log
