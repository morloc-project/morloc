all:
	morloc make main.loc 2> build.err
	./nexus x > obs.txt 2> obs.err
	./nexus y 2>> obs.err  >> obs.txt
	./nexus z 2>> obs.err  >> obs.txt
	./nexus f 99 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus* pool* log __pycache__
