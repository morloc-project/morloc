all:
	rm -f *.err obs.txt
	rm -f /tmp/ostream-implicit-*.dat
	morloc make -o nexus main.loc 2> build.err
	# === writeAndFlush path ===
	# Writer commits three sub-packets via @flush, exits without
	# @close. The per-call sweep releases the slot. Readers see
	# all three flushed sub-packets.
	./nexus writeAndFlush          "/tmp/ostream-implicit-a.dat" >> obs.txt 2>> obs.err
	./nexus countViaIStream        "/tmp/ostream-implicit-a.dat" >> obs.txt 2>> obs.err
	./nexus drainFirstSubpacket    "/tmp/ostream-implicit-a.dat" >> obs.txt 2>> obs.err
	./nexus drainSecondSubpacket   "/tmp/ostream-implicit-a.dat" >> obs.txt 2>> obs.err
	./nexus drainThirdSubpacket    "/tmp/ostream-implicit-a.dat" >> obs.txt 2>> obs.err
	./nexus drainPastEnd           "/tmp/ostream-implicit-a.dat" >> obs.txt 2>> obs.err
	# === writeWithoutFlush path ===
	# Writer never flushes or closes; buffered data is dropped.
	# Reader sees zero sub-packets on disk.
	./nexus writeWithoutFlush      "/tmp/ostream-implicit-b.dat" >> obs.txt 2>> obs.err
	./nexus countViaIStream        "/tmp/ostream-implicit-b.dat" >> obs.txt 2>> obs.err
	./nexus drainAfterNoFlush      "/tmp/ostream-implicit-b.dat" >> obs.txt 2>> obs.err
	rm -f /tmp/ostream-implicit-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/ostream-implicit-*.dat
