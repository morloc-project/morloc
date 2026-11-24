all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus given 4 >> obs.txt 2>> obs.err
	echo '#include "give.hpp"' | $${CXX:-g++} -std=c++20 -fsyntax-only -x c++ -I. - 2>> obs.err && echo "alone ok" >> obs.txt

clean:
	rm -rf nexus pools __pycache__
	rm -f *.err
	rm -rf *-build *build.tmp*
