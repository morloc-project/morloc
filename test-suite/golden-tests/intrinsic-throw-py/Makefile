all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus plainThrow            2>  obs.err ; echo "exit:$$?" >> obs.txt
	grep -oE "PLAIN: boom"        obs.err       >> obs.txt
	./nexus interpThrow "hello"   2>> obs.err ; echo "exit:$$?" >> obs.txt
	grep -oE "INTERP: hello n=5"  obs.err       >> obs.txt
	./nexus guardThrow "hi"       2>> obs.err ; echo "exit:$$?" >> obs.txt
	grep -oE "GUARD: too short n=2" obs.err     >> obs.txt
	./nexus guardThrow "hello"    >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt

clean:
	rm -rf nexus pools *.err
