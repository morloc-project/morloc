all:
	rm -f obs.txt z*
	morloc make foo.loc 2> build.err
	# 0
	./nexus -f json f0 '[1,2,3,4,5,6]' > z0.json
	./nexus -f mpk f0 z0.json > z0.mpk
	./nexus -f voidstar f0 z0.mpk > z0.dat
	./nexus f0 z0.mpk > obs.txt 2> obs.err
	# 1
	./nexus -f json f1 '[["abc",5],["def",6]]' > z1.json
	./nexus -f mpk f1 z1.json > z1.mpk
	./nexus -f voidstar f1 z1.mpk > z1.dat
	./nexus f1 z1.mpk 2>> obs.err  >> obs.txt
	# 2
	./nexus -f json f2 '[["abc","def"],[5,6]]' > z2.json
	./nexus -f mpk f2 z2.json > z2.mpk
	./nexus -f voidstar f2 z2.mpk > z2.dat
	./nexus f2 z2.mpk 2>> obs.err  >> obs.txt
	# 3
	./nexus -f json f3 '[[[[9,9,9],[1,2],7],[[4,4,4,4],[3,4,5],8]],9]' > z3.json
	./nexus -f mpk f3 z3.json > z3.mpk
	./nexus -f voidstar f3 z3.mpk > z3.dat
	./nexus f3 z3.mpk 2>> obs.err  >> obs.txt
	# 4
	./nexus -f json f4 '[[[6,7]]]' > z4.json
	./nexus -f mpk f4 z4.json > z4.mpk
	./nexus -f voidstar f4 z4.mpk > z4.dat
	./nexus f4 z4.mpk 2>> obs.err  >> obs.txt
	# 5
	./nexus -f json f5 '[[[["abc","defg"],[1,2],7],[["hijkl","mnopqrs"],[3,4,5],8]],9]' > z5.json
	./nexus -f mpk f5 z5.json > z5.mpk
	./nexus -f voidstar f5 z5.mpk > z5.dat
	./nexus f5 z5.mpk 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus* pool* z* __pycache__
