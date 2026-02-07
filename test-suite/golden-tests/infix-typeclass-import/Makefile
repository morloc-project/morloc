all:
	morloc make main.loc
	./nexus test_comp > obs.txt
	./nexus test_min >> obs.txt
	./nexus test_not_equal >> obs.txt

clean:
	rm -f nexus* pool* log
