all:
	rm -f *.err obs.txt
	rm -f /tmp/wire-schema-no-hints.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus saveVec /tmp/wire-schema-no-hints.dat  >  obs.txt 2>  obs.err
	# The metadata section starts at offset 32 (right after the 32-byte
	# packet header). Its first 8 bytes are the metadata-entry header
	# (`mmh` magic + type byte + u32 size); the schema string starts at
	# offset 40. Scan the metadata region for a `<` byte -- its presence
	# means a hint prefix leaked back onto the wire.
	echo -n "wire-has-hint: " >> obs.txt
	if od -An -c -j 40 -N 128 /tmp/wire-schema-no-hints.dat | grep -q '<' ; then \
	  echo "YES" >> obs.txt ; \
	else \
	  echo "NO" >> obs.txt ; \
	fi
	rm -f /tmp/wire-schema-no-hints.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/wire-schema-no-hints.dat
