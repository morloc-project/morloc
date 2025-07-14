all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	# check [(Str,Int)]
	./nexus -f json f1 '[["abc",5],["def",6]]' > z1.json
	./nexus -f mpk f1 z1.json > z1.mpk
	./nexus -f packet f1 z1.dat > z1.dat
	./nexus f1 z1.mpk > obs
	# check ([Str],[Int])
	./nexus -f json f2 '[["abc","def"],[5,6]]' > z2.json
	./nexus -f mpk f2 z2.json > z2.mpk
	./nexus -f packet f2 z2.mpk > z2.dat
	./nexus f2 z2.dat >> obs

clean:
	rm -rf nexus* pool* z1* z2* __pycache__
