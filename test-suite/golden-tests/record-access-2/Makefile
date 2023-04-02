all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus.pl foo '{"name":["alice","bob"],"age":34}' > obs.txt

clean:
	rm -f nexus* pool*
