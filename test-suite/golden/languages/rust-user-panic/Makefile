all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus nth '[1,2,3]' 1 >> obs.txt 2> obs.err
	./nexus safeNth '[1,2,3]' 9 >> obs.txt 2>> obs.err
	./nexus afterPanic '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus nth '[1,2,3]' 9 2> panic.err ; echo "exit:$$?" >> obs.txt
	grep -oE "Rust code panicked" panic.err | head -1 >> obs.txt
	grep -c "Pool process exited" panic.err >> obs.txt || true

clean:
	rm -rf nexus pools *.err
	rm -rf *-build *build.tmp*
