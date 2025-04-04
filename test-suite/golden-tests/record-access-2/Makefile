all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus foo '[{"name":"alice","age":34}, {"name":"bob","age":36}]' > obs.txt

clean:
	rm -f nexus* pool*
