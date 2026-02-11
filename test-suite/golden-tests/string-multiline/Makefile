all:
	morloc make main.loc 2> build.err
	./nexus s1 >  obs.txt
	./nexus s2 2>> obs.err  >> obs.txt
	./nexus s3 2>> obs.err  >> obs.txt
	./nexus s4 2>> obs.err  >> obs.txt

clean:
	rm -f nexus*
