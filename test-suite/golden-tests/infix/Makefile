all:
	morloc make main.loc
	./nexus test_arithmetic       > obs.txt
	./nexus test_precedence       >> obs.txt
	./nexus test_exponentiation   >> obs.txt
	./nexus test_append           >> obs.txt
	./nexus test_application      >> obs.txt

clean:
	rm -rf pool* nexus* __pycache__
