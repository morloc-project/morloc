all:
	rm -f *.err obs.txt
	morloc make -o nexus t0.loc 2> build.err
	./nexus checkInt 2>> obs.err  >> obs.txt
	./nexus checkInt8 2>> obs.err  >> obs.txt
	./nexus checkInt16 2>> obs.err  >> obs.txt
	./nexus checkInt32 2>> obs.err  >> obs.txt
	./nexus checkInt64 2>> obs.err  >> obs.txt
	./nexus checkUInt8 2>> obs.err  >> obs.txt
	./nexus checkUInt16 2>> obs.err  >> obs.txt
	./nexus checkUInt32 2>> obs.err  >> obs.txt
	./nexus checkUInt64 2>> obs.err  >> obs.txt
	./nexus checkReal 2>> obs.err  >> obs.txt
	./nexus checkBool 2>> obs.err  >> obs.txt
	./nexus checkStr 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus pools
