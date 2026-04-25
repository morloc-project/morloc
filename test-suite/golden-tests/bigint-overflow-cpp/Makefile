all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus factCpp 5 > obs.txt 2>> obs.err
	./nexus factCpp 100 >> obs.txt 2>> obs.err; echo "exit:$$?" >> obs.txt

build:
	g++ -O2 -o pools/main/pool-cpp.out pools/main/pool.cpp -std=c++17 -include morloc_pch.hpp -I/home/dev/.local/share/morloc/include -L/home/dev/.local/share/morloc/lib -Wl,-rpath,/home/dev/.local/share/morloc/lib -lmorloc -lcppmorloc -lpthread -I. -I/home/dev/.local/share/morloc/src/morloc/plane/default/root-cpp

clean:
	rm -rf nexus pools *err
