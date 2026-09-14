CMDS = same sameTwice renamed sliced

all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	for c in $(CMDS); do echo "--- $$c ---" >> obs.txt; ./nexus $$c 1 >> obs.txt 2>> obs.err; echo "--- $$c no-borrow ---" >> obs.txt; MORLOC_ARROW_NO_BORROW=1 ./nexus $$c 1 >> obs.txt 2>> obs.err; done

clean:
	rm -rf nexus pools *.err __pycache__
	rm -rf *-build *build.tmp*
