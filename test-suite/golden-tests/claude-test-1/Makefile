all:
	rm -f obs.txt
	morloc make main.loc
	./nexus addTwo 5 > obs.txt
	./nexus doubleList '[1,2,3,4]' >> obs.txt

clean:
	rm -f nexus* pool*
