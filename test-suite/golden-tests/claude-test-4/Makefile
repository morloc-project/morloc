all:
	rm -f obs.txt
	morloc make main.loc > /dev/null
	./nexus processNumbers '[1,2,3]' > obs.txt
	./nexus composeMaps '[5.0,10.0]' >> obs.txt
	./nexus nestedLambda 10 '[1,2,3]' >> obs.txt

clean:
	rm -f nexus* pool*
