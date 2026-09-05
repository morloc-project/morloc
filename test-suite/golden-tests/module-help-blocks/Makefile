all:
	rm -f *.err obs.txt
	morloc make -o tool tool.loc 2> build.tool.err
	morloc make -o solo solo.loc 2> build.solo.err
	printf -- '--- multi short ---\n' > obs.txt
	./tool -h >> obs.txt 2>> obs.err
	printf -- '--- multi long description ---\n' >> obs.txt
	./tool --help 2>> obs.err | sed -n '1,/^Usage:/p' >> obs.txt
	printf -- '--- single short ---\n' >> obs.txt
	./solo -h >> obs.txt 2>> obs.err

clean:
	rm -rf tool solo pools log __pycache__ *.err
	rm -rf *-build *build.tmp*
