all:
	rm -f *.err
	: > obs.txt
	morloc make -X rust:lto=thin -o nexus main.loc 2> build.err
	./nexus weave 4 >> obs.txt 2> obs.err
	./nexus weave 0 2> weave.err ; echo "exit:$$?" >> obs.txt
	grep -oE "exited with status [0-9]+" weave.err | head -1 >> obs.txt
	./nexus safeWeave 0 2> safe.err ; echo "exit:$$?" >> obs.txt
	grep -oE "exited with status [0-9]+" safe.err | head -1 >> obs.txt

clean:
	rm -rf nexus pools *.err
	rm -rf *-build *build.tmp*
