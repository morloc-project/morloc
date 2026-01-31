all:
	rm -f obs.txt
	morloc make main.loc
	./nexus test_arithmetic       > obs.txt
	./nexus test_precedence       >> obs.txt
	./nexus test_exponentiation   >> obs.txt
	./nexus test_cons             >> obs.txt
	./nexus test_append           >> obs.txt
	./nexus test_application      >> obs.txt
	./nexus test_combined         >> obs.txt

clean:
	rm -rf pool* nexus* __pycache__
