all:
	morloc make main.loc
	./nexus test_add 5 > obs.txt
	./nexus test_mul 5 >> obs.txt
	./nexus test_expr >> obs.txt
	./nexus test_negate >> obs.txt

clean:
	rm -f nexus* pool* log
