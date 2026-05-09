all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus test 2>> obs.err  >> obs.txt
	morloc make -o bad_hex bad_hex.loc 2>&1 \
	  | sed 's|.*bad_hex\.loc|bad_hex.loc|' \
	  | grep -F 'malformed hexadecimal literal' >> obs.txt || true
	morloc make -o bad_oct bad_oct.loc 2>&1 \
	  | sed 's|.*bad_oct\.loc|bad_oct.loc|' \
	  | grep -F 'malformed octal literal' >> obs.txt || true
	morloc make -o bad_bin bad_bin.loc 2>&1 \
	  | sed 's|.*bad_bin\.loc|bad_bin.loc|' \
	  | grep -F 'malformed binary literal' >> obs.txt || true

clean:
	rm -rf nexus bad_hex bad_oct bad_bin pools
