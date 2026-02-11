all:
	morloc make t0.loc 2> build.err
	rm -f obs.txt
	./nexus checkInt 2>> obs.err  >> obs.txt
	./nexus checkSInt8 2>> obs.err  >> obs.txt
	./nexus checkSInt16 2>> obs.err  >> obs.txt
	./nexus checkSInt32 2>> obs.err  >> obs.txt
	./nexus checkSInt64 2>> obs.err  >> obs.txt
	./nexus checkUInt8 2>> obs.err  >> obs.txt
	./nexus checkUInt16 2>> obs.err  >> obs.txt
	./nexus checkUInt32 2>> obs.err  >> obs.txt
	./nexus checkUInt64 2>> obs.err  >> obs.txt
	./nexus checkReal 2>> obs.err  >> obs.txt
	./nexus checkBool 2>> obs.err  >> obs.txt
	./nexus checkStr 2>> obs.err  >> obs.txt

clean:
	rm -f nexus*
