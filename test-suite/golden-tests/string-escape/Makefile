all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	>obs.txt
	./nexus pyQ    2>> obs.err >> obs.txt
	./nexus cppQ   2>> obs.err >> obs.txt
	./nexus rQ     2>> obs.err >> obs.txt
	./nexus pyAll  2>> obs.err >> obs.txt
	./nexus cppAll 2>> obs.err >> obs.txt
	./nexus rAll   2>> obs.err >> obs.txt
	./nexus pureQ  2>> obs.err >> obs.txt
	./nexus pureAll 2>> obs.err >> obs.txt
	./nexus pyNul     2>> obs.err >> obs.txt
	./nexus cppNul    2>> obs.err >> obs.txt
	./nexus pyNulLen  2>> obs.err >> obs.txt
	./nexus cppNulLen 2>> obs.err >> obs.txt
	./nexus pureNul   2>> obs.err >> obs.txt
	morloc make -o bad bad.loc 2>&1 \
	  | sed 's|.*bad\.loc|bad.loc|' \
	  | grep -F 'invalid escape sequence' >> obs.txt || true

clean:
	rm -rf nexus bad pools __pycache__
