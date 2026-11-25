all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus fooCpp '{"name":"a","age":3,"inner":{"label":"x","tags":["t"]}}' > obs.txt 2> obs.err
	./nexus age '{"name":"a","age":3,"inner":{"label":"x","tags":["t"]}}' >> obs.txt 2>> obs.err
	./nexus area '{"Rect":[2,3]}' >> obs.txt 2>> obs.err
	./nexus area '{"Circle":[1]}' >> obs.txt 2>> obs.err
	grep '^#include' nexus-build/pools/types/cpp/mlc/types/_local/main.hpp >> obs.txt

clean:
	rm -rf nexus pools *err *-build *build.tmp*
