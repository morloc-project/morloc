all:
	rm -f obs.txt
	morloc make foo.loc
	./nexus.pl square 16 >> obs.txt
	./nexus.pl rms [1,2,3] >> obs.txt
	./nexus.pl sqrtsum [1,2,3] >> obs.txt
	./nexus.pl nested_map [1,2,3] >> obs.txt


clean:
	rm -f nexus* pool* *gch
