all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus catchListDoFb "hi"      >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchListDoFb ""        >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchListDoFbOpt "hi"   >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchListDoFbOpt ""     >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchLetDoFb "hi"       >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchLetDoFb ""         >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt

clean:
	rm -rf nexus pools *-build __pycache__ *.err
