all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus tryNested 5 >> obs.txt 2> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus tryText 5 >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus tryAfterText 5 >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus tryCurried 5 >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus ok 0 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
