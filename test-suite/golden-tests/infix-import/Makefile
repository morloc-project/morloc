all:
	rm -f obs.txt
	morloc make main.loc 
	./nexus test_addition >> obs.txt
	./nexus test_multiplication >> obs.txt
	./nexus test_exponentiation >> obs.txt
	./nexus test_concat >> obs.txt
	./nexus test_combined >> obs.txt

clean:
	rm -f nexus* pool* log
