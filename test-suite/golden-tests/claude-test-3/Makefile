all:
	rm -f obs.txt
	morloc make main.loc
	./nexus threeLangChain 5 > obs.txt
	./nexus reverseChain 25 >> obs.txt

clean:
	rm -f nexus* pool*
