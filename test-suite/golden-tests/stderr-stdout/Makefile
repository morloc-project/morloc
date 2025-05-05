all:
	rm -f obs.txt
	morloc make main.loc
	./nexus pfoo '"wena"' 2> obs.txt >> obs.txt
	./nexus rfoo '"wena"' 2>> obs.txt >> obs.txt
	./nexus cfoo '"wena"' 2>> obs.txt >> obs.txt


clean:
	rm -rf nexus* pool* log __pycache__
