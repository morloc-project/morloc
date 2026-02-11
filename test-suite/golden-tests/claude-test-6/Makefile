all:
	rm -f obs.txt
	morloc make main.loc 2> build.err
	./nexus blur --width 10 --height 10 > obs.txt 2> obs.err
	./nexus edges --width 10 --height 10 2>> obs.err  >> obs.txt
	./nexus analyze --width 8 --height 8 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
