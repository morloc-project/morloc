all:
	rm -f *.err obs.txt
	morloc make -o nexus foo.loc 2> build.err
	# g++ -O3 --std=c++17 -ocpp.out pool.cpp -std=c++17 -I/home/z/.morloc/include -I.
	./nexus foo [7,8,9] > obs.txt 2> obs.err

clean:
	rm -rf nexus __pycache__
