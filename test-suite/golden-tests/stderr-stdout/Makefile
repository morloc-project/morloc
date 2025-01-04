all:
	rm -f obs.txt
	morloc make main.loc
	./nexus.py pfoo '"wena"' 2> obs.txt >> obs.txt
	./nexus.py rfoo '"wena"' 2>> obs.txt >> obs.txt
	./nexus.py cfoo '"wena"' 2>> obs.txt >> obs.txt


clean:
	rm -rf nexus* pool* log __pycache__
