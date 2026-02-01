obs.txt: nexus
	./nexus test_arithmetic       > obs.txt
	./nexus test_precedence       >> obs.txt
	./nexus test_exponentiation   >> obs.txt
	./nexus test_append           >> obs.txt
	./nexus test_application      >> obs.txt

nexus: main.loc impl.py
	morloc make main.loc

clean:
	rm -rf pool* nexus* __pycache__
