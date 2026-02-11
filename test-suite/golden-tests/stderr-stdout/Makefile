all:
	rm -f obs.txt
	morloc make main.loc 2> build.err
	./nexus pfoo '"wena"' 2> obs.txt 2>> obs.err  >> obs.txt
	./nexus rfoo '"wena"' 2>> obs.txt 2>> obs.err  >> obs.txt
	./nexus cfoo '"wena"' 2>> obs.txt 2>> obs.err  >> obs.txt


clean:
	rm -rf nexus* pool* log __pycache__
