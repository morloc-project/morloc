all:
	rm -f obs.txt
	morloc make main.loc 
	./nexus foo '[6,5]' > obs.txt
	./nexus bar '[6,5]' >> obs.txt

clean:
	rm -f nexus* pool*
