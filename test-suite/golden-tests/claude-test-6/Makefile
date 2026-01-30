all:
	rm -f obs.txt
	morloc make main.loc
	./nexus blur --width 10 --height 10 > obs.txt
	./nexus edges --width 10 --height 10 >> obs.txt
	./nexus analyze --width 8 --height 8 >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
