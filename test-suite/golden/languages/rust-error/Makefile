all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus checkLen '[1,2,3]'  >> obs.txt 2> obs.err
	./nexus catchBare           >> obs.txt 2>> obs.err
	./nexus safeCheck '[1]'     >> obs.txt 2>> obs.err
	./nexus safeCheck '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus checkLen '[1]' 2> throw.err ; echo "exit:$$?" >> obs.txt
	grep -oE "checkLen: list too short" throw.err >> obs.txt
	grep -oE "\[rust\]" throw.err | head -1 >> obs.txt
	./nexus crossThrow '[1]' 2> cross.err ; echo "exit:$$?" >> obs.txt
	grep -oE "\[rust\]" cross.err | head -1 >> obs.txt
	grep -oE "\[py\]" cross.err | head -1 >> obs.txt

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
