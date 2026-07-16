all:
	: > obs.txt
	: > obs.err
	morloc make -o nexus main.loc 2> build.err
	./nexus r1     >> obs.txt 2>> obs.err
	./nexus r2     >> obs.txt 2>> obs.err
	./nexus r3     >> obs.txt 2>> obs.err
	./nexus r4     >> obs.txt 2>> obs.err
	./nexus r5     >> obs.txt 2>> obs.err
	./nexus ys     >> obs.txt 2>> obs.err
	./nexus f32Ok  >> obs.txt 2>> obs.err
	morloc make -o bad_overflow bad_overflow.loc 2>&1 \
	  | sed 's|.*bad_overflow\.loc|bad_overflow.loc|' \
	  | grep -F 'overflows Float32' >> obs.txt || true

clean:
	rm -rf nexus bad_overflow pools
