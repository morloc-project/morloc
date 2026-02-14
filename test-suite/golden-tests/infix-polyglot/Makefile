all:
	rm -f obs.txt *.err
	morloc make main.loc 2> build.err
	./nexus test_sum_py > obs.txt 2> obs.err
	./nexus test_product_py 2>> obs.err  >> obs.txt
	./nexus test_chain_py 2>> obs.err  >> obs.txt
	./nexus test_sum_cpp 2>> obs.err  >> obs.txt

clean:
	rm -f nexus* pool* log
