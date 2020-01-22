# Files that should exist:
# - Makefile
# - doubleSerial.hpp
# - partial-01.hpp
# - partial-01.loc

all:
	rm -f obs.txt
	morloc make partial-01.loc
	./nexus.pl foo 16 > obs.txt

clean:
	rm -f nexus* pool*
