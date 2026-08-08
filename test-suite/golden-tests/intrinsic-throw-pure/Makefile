all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus throwInt 42           2>  obs.err ; echo "exit:$$?" >> obs.txt
	grep -oE "PURE: boom int"     obs.err       >> obs.txt
	./nexus throwUnit 42          2>> obs.err ; echo "exit:$$?" >> obs.txt
	grep -oE "PURE: boom unit"    obs.err       >> obs.txt
	./nexus caughtInt 7           >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt

clean:
	rm -rf nexus pools *.err
