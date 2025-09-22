all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus test > obs.txt

clean:
	rm -f nexus* pool* *pdf
