all:
	rm -f obs.txt
	morloc make main.loc
	./nexus pythonToCpp 3 2 > obs.txt
	./nexus cppToPython 4 7 >> obs.txt

clean:
	rm -f nexus* pool*
