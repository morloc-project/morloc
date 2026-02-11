all:
	rm -f obs.txt
	morloc make main.loc 2> build.err
	./nexus pythonToCpp 3 2 > obs.txt 2> obs.err
	./nexus cppToPython 4 7 2>> obs.err  >> obs.txt

clean:
	rm -f nexus* pool*
