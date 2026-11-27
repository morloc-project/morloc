# Verify the runtime rejects a NUL-bearing string at the R boundary.
# The function reads its Str argument from a file (`source: file`),
# the file contains a literal NUL byte, and the R guard fires.

all:
	rm -f *.err obs.txt nul.txt
	morloc make -o nexus main.loc 2> build.err
	printf 'abc\000def' > nul.txt
	./nexus rEcho nul.txt > /dev/null 2> obs.err && echo 'RC=0' > obs.txt || echo 'RC<>0' > obs.txt

clean:
	rm -rf nexus pools __pycache__ nul.txt
	rm -rf *-build *build.tmp*
