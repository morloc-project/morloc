all:
	morloc make main.loc
	./nexus test_add > obs.txt
	./nexus test_mul >> obs.txt
	./nexus test_expr >> obs.txt
	./nexus test_negate >> obs.txt

clean:
	rm -f nexus* pool* log
