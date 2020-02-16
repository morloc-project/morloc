# Files that should exist:
# - Makefile
# - doubleSerial.hpp
# - partial-01.hpp
# - partial-01.loc

all:
	rm -f obs.txt
	morloc make foo.loc
	./nexus.pl r2cpp 5 > obs.txt

clean:
	rm -f nexus* pool*
