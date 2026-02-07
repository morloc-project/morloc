all:
	morloc make main.loc
	./nexus test_sum_py > obs.txt
	./nexus test_product_py >> obs.txt
	./nexus test_chain_py >> obs.txt
	./nexus test_sum_cpp >> obs.txt

clean:
	rm -f nexus* pool* log obs.txt
