all:
	morloc make t0.loc 
	rm -f obs.txt
	./nexus checkInt >> obs.txt
	./nexus checkSInt8 >> obs.txt
	./nexus checkSInt16 >> obs.txt
	./nexus checkSInt32 >> obs.txt
	./nexus checkSInt64 >> obs.txt
	./nexus checkUInt8 >> obs.txt
	./nexus checkUInt16 >> obs.txt
	./nexus checkUInt32 >> obs.txt
	./nexus checkUInt64 >> obs.txt
	./nexus checkReal >> obs.txt
	./nexus checkBool >> obs.txt
	./nexus checkStr >> obs.txt

clean:
	rm -f nexus*
