all:
	rm -f *.err obs.txt
	morloc make -o ok ok.loc 2> build.err
	morloc make -o bool_arg bool_arg.loc >> obs.txt 2>> obs.txt || true
	morloc make -o two_stdin two_stdin.loc >> obs.txt 2>> obs.txt || true
	morloc make -o late_stdin late_stdin.loc >> obs.txt 2>> obs.txt || true

clean:
	rm -rf ok bool_arg two_stdin late_stdin pools log __pycache__ *.err
	rm -rf *-build *build.tmp*
