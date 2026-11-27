all:
	rm -f *.err obs.txt
	morloc make -o tool tool.loc 2> build.tool.err
	morloc make -o solo solo.loc 2> build.solo.err
	morloc make -o modonly modonly.loc 2> build.modonly.err
	morloc make -o cmdonly cmdonly.loc 2> build.cmdonly.err
	printf -- '--- multi -h ---\n' > obs.txt
	./tool -h >> obs.txt 2>> obs.err
	printf -- '--- multi -hh ---\n' >> obs.txt
	./tool -hh >> obs.txt 2>> obs.err
	printf -- '--- multi --help --help ---\n' >> obs.txt
	./tool --help --help >> obs.txt 2>> obs.err
	printf -- '--- multi subcommand -h ---\n' >> obs.txt
	./tool greet -h >> obs.txt 2>> obs.err
	printf -- '--- multi subcommand -hh ---\n' >> obs.txt
	./tool greet -hh >> obs.txt 2>> obs.err
	printf -- '--- multi subcommand -hhh ---\n' >> obs.txt
	./tool describe -hhh >> obs.txt 2>> obs.err
	printf -- '--- single -h ---\n' >> obs.txt
	./solo -h >> obs.txt 2>> obs.err
	printf -- '--- single -hh ---\n' >> obs.txt
	./solo -hh >> obs.txt 2>> obs.err
	printf -- '--- module description only -hh ---\n' >> obs.txt
	./modonly -hh >> obs.txt 2>> obs.err
	printf -- '--- command description only -hh ---\n' >> obs.txt
	./cmdonly -hh >> obs.txt 2>> obs.err

clean:
	rm -rf tool solo modonly cmdonly pools log __pycache__ *.err
	rm -rf *-build *build.tmp*
