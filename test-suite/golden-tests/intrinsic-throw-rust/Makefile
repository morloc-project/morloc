all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	if grep -q "^warning" build.err; then echo "UNEXPECTED: build warnings" >> obs.txt; else echo "OK: clean build" >> obs.txt; fi
	./nexus tailThrow "hello"        2>  obs.err ; echo "exit:$$?" >> obs.txt
	grep -oE "TAIL: hello n=5"       obs.err       >> obs.txt
	./nexus tailThrowList "hello"    2>> obs.err ; echo "exit:$$?" >> obs.txt
	grep -oE "TAILLIST"              obs.err       >> obs.txt
	./nexus guardThrow "hi"          2>> obs.err ; echo "exit:$$?" >> obs.txt
	grep -oE "GUARD: too short n=2"  obs.err       >> obs.txt
	./nexus guardThrow "hello"       >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus armsThrow "a"            2>> obs.err ; echo "exit:$$?" >> obs.txt
	grep -oE "ARMS: short"           obs.err       >> obs.txt
	./nexus armsThrow "abc"          2>> obs.err ; echo "exit:$$?" >> obs.txt
	grep -oE "ARMS: long"            obs.err       >> obs.txt
	./nexus unwrapT "hello"          2>> obs.err ; echo "exit:$$?" >> obs.txt
	grep -oE "TAIL: hello n=5"       obs.err | tail -1 >> obs.txt
	./nexus safeArms "abc"           >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt

clean:
	rm -rf nexus *.err
	rm -rf *-build *build.tmp*
