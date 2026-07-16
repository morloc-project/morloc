all:
	rm -f *.err obs.txt
	morloc make -o nexus t0.loc 2> build.err
	./nexus checkInt 2>> obs.err  >> obs.txt
	./nexus checkI8 2>> obs.err  >> obs.txt
	./nexus checkI16 2>> obs.err  >> obs.txt
	./nexus checkI32 2>> obs.err  >> obs.txt
	./nexus checkI64 2>> obs.err  >> obs.txt
	./nexus checkU8 2>> obs.err  >> obs.txt
	./nexus checkU16 2>> obs.err  >> obs.txt
	./nexus checkU32 2>> obs.err  >> obs.txt
	./nexus checkU64 2>> obs.err  >> obs.txt
	./nexus checkReal 2>> obs.err  >> obs.txt
	./nexus checkBool 2>> obs.err  >> obs.txt
	./nexus checkStr 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus pools
