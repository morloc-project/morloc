all:
	morloc make main.loc 
	./nexus s1 >  obs.txt
	./nexus s2 >> obs.txt
	./nexus s3 >> obs.txt
	./nexus s4 >> obs.txt

clean:
	rm -f nexus*
