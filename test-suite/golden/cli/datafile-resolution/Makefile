all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus purePath > pure.out 2>> obs.err
	./nexus poolPath > pool.out 2>> obs.err
	if cmp -s pure.out pool.out && grep -q "^\"\\\\/.*data.txt\"$$" pure.out; then echo OK > obs.txt; else echo UNEXPECTED > obs.txt; fi

clean:
	rm -rf nexus pools __pycache__ *.err *.out
	rm -rf *-build *build.tmp*
