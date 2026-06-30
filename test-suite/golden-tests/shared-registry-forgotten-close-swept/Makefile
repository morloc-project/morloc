all:
	rm -f *.err obs.txt
	rm -f /tmp/shared-registry-leak-*.dat
	morloc make -o nexus main.loc 2> build.err
	# === leakNoFlush path ===
	# Writer exits without flushing or closing. Per-call sweep
	# drops the buffer. On-disk file has no sub-packets.
	./nexus leakNoFlush          "/tmp/shared-registry-leak-a.dat" >> obs.txt 2>> obs.err
	./nexus observeNoFlush       "/tmp/shared-registry-leak-a.dat" >> obs.txt 2>> obs.err
	./nexus observeNoFlushNext   "/tmp/shared-registry-leak-a.dat" >> obs.txt 2>> obs.err
	# === leakWithFlush path ===
	# Writer flushed before exit. Per-call sweep just releases
	# the slot; the flushed sub-packet survives.
	./nexus leakWithFlush        "/tmp/shared-registry-leak-b.dat" >> obs.txt 2>> obs.err
	./nexus observeWithFlush     "/tmp/shared-registry-leak-b.dat" >> obs.txt 2>> obs.err
	./nexus observeWithFlushNext "/tmp/shared-registry-leak-b.dat" >> obs.txt 2>> obs.err
	rm -f /tmp/shared-registry-leak-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/shared-registry-leak-*.dat
