all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus threeLangChain 5 > obs.txt 2> obs.err
	./nexus reverseChain 25 2>> obs.err  >> obs.txt

clean:
	rm -f nexusgc
