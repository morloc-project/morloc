all:
	rm -f *.err obs.txt
	morloc make -o tool tool.loc 2> build.tool.err
	morloc make -o solo solo.loc 2> build.solo.err
	morloc make -o modonly modonly.loc 2> build.modonly.err
	morloc make -o cmdonly cmdonly.loc 2> build.cmdonly.err
	printf -- '--- multi short ---\n' > obs.txt
	./tool -h >> obs.txt 2>> obs.err
	printf -- '--- multi long description ---\n' >> obs.txt
	./tool --help 2>> obs.err | sed -n '1,/^Usage:/p' >> obs.txt
	printf -- '--- single short ---\n' >> obs.txt
	./solo -h >> obs.txt 2>> obs.err
	printf -- '--- single long description ---\n' >> obs.txt
	./solo --help 2>> obs.err | sed -n '1,/^Usage:/p' >> obs.txt
	printf -- '--- module description only ---\n' >> obs.txt
	./modonly -h >> obs.txt 2>> obs.err
	printf -- '--- command description only ---\n' >> obs.txt
	./cmdonly -h >> obs.txt 2>> obs.err

clean:
	rm -rf tool solo modonly cmdonly pools log __pycache__ *.err
	rm -rf *-build *build.tmp*
