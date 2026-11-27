all:
	: > obs.txt
	rm -f *.err
	morloc make -o pure pure.loc 2>> build.err
	./pure applied 1 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./pure caught 1 >> obs.txt 2>> obs.err
	./pure unusedLet 1 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./pure unusedWhere 1 >> obs.txt 2>> obs.err
	morloc make -o py py.loc 2>> build.err
	./py applied 1 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./py caught 1 >> obs.txt 2>> obs.err
	./py unusedLet 1 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./py unusedWhere 1 >> obs.txt 2>> obs.err
	morloc make -o cpp cpp.loc 2>> build.err
	./cpp applied 1 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./cpp caught 1 >> obs.txt 2>> obs.err
	./cpp unusedLet 1 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./cpp unusedWhere 1 >> obs.txt 2>> obs.err
	grep -c "FT: applied" obs.err >> obs.txt
	grep -c "FT: let" obs.err >> obs.txt

clean:
	rm -rf pure py cpp *.err
	rm -rf *-build *build.tmp*
