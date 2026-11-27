all:
	rm -f *.err obs.txt
	MORLOC_LANG_PARAMS=rust:lto=off morloc make -o nexus main.loc 2> build.err
	./nexus twice 3 > /dev/null 2> run.err; echo "exit $$?" > obs.txt
	grep -c 'exited with status 70' run.err >> obs.txt
	grep -c 'abort came while a panic was unwinding' run.err >> obs.txt

clean:
	rm -rf nexus pools *-build *build.tmp* target
